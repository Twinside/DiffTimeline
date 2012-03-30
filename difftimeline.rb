#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-

unless Kernel.respond_to?(:require_relative)
  module Kernel
    def require_relative(path)
      require File.join(File.dirname(caller[0]), path.to_str)
    end
  end
end

require 'rubygems'
require "bundler/setup"

require 'net/http/server'
require 'launchy'
require 'pp'
require 'pathname'
require 'json'
require 'tmpdir'
require 'rbconfig'

require_relative 'lib/diff'
require_relative 'lib/objects'
require_relative 'lib/cached_repository'
require_relative 'lib/diffset'

if ARGV.size <= 0
    puts "Error: no specified file"
    puts "   syntax : ruby diffTimeline.rb file"
    exit(1)
end

class RepositoryNotFound < RuntimeError
end

class QueryingError < RuntimeError
end

class FileNotInRepository < RuntimeError
    attr_reader :filename
    def initialize(f)
        @filename = f
    end
end

class DiffTimelineState
    def initialize
        @is_client_launched = false
        @exec_path = Pathname(__FILE__).realpath.parent
        @is_app = ARGV[0] == '--app'

        if RUBY_VERSION =~ /^1\.8.*/
            @is_1_8 = true
        else
            @is_1_8 = false
        end

        if @is_app
            puts 'Launching in app mode'
            requested_filename = Pathname.new(ARGV[1]).basename
            @current_dir = Pathname.new(ARGV[1]).parent.cleanpath
        else
            requested_filename = Pathname.new(ARGV[0])
            @current_dir = Pathname.new(Dir.pwd)
        end

        repo_dir = find_nearest_git_repo()

        raise RepositoryNotFound if repo_dir.nil?

        puts "> Git repository : #{repo_dir}"
        @tracked_path = (@current_dir + requested_filename).cleanpath.relative_path_from(Pathname.new(repo_dir))
        @repository = GitRead::CachedRepository.new(repo_dir)
        if RbConfig::CONFIG['host_os'] =~ /mswin|mingw/
            @codeoverview_exec = 'C:\\Users\\Vince\\vimfiles\\bundle\\vim-codeoverview\\plugin\\codeoverview.exe'
        else
            @codeoverview_exec = '~/.vim/bundle/vim-codeoverview/plugin/codeoverview'
        end
    end

    def exec_path
        @exec_path
    end

    def launch_client(port)
        if !@is_client_launched
            puts 'Launching client'
            # Serious race condition, but it will be ok to test
            Launchy.open("http://127.0.0.1:#{port}")
            @is_client_launched = true
        end
    end

    def find_nearest_git_repo
        currDir = Dir.pwd
        Pathname.new(@current_dir).ascend do |v|
            gitDir = v + '.git'
            return v if File.exists?(gitDir)
        end
    end

    def send_error_message(what)
        response = { "error" => what }

        [200, {'Content-Type' => 'text/json'}, [response.to_json]]
    end

    def split_query_string(str)
        rez = {}

        str.split('&').each do |param|
            splits = param.split('=')
            if splits.size == 2
                rez[splits[0]] = splits[1]
            end
        end

        rez
    end

    def find_first_commit(file_path, commit_path, sha_file, last_commit, commit_sha)
        commit = @repository.access_object(commit_sha)
        return last_commit if commit.nil?

        file = commit.tree.access_path(file_path)
        return last_commit if file.nil?

        return last_commit if sha_file != file.sha
        prev_commit = commit.parents_sha[0]
        commit_path << {
            "commit" => last_commit.sha,
            "parent_commit" => commit.sha,
            "message" => last_commit.to_s
        }

        find_first_commit(file_path, commit_path, sha_file, commit, commit.parents_sha[0])
    end

    # Given a query string, find the first commit with a different
    # file.
    # yield commit_path, commit, last_file, file
    def yield_query(query)

        if query['commit'] == nil || query['last_file'] == nil
            raise QueryingError, send_error_message('Invalid query')
        end
        puts ">> Asking #{query['commit']}"

        prev_file_sha = GitRead::ShaRef.new(query['last_file'])
        prev_commit = GitRead::ShaRef.new(query['commit'])
        commit_path = []
        file_path = query['path']
        file = nil
        keep_digging = true

        begin
            commit = @repository.access_object(prev_commit)

            if commit.nil?
                puts "Commit not found #{prev_commit}"
                raise QueryingError, send_error_message("Commit not found #{prev_commit}")
            elsif commit.class != GitRead::Commit
                puts "#{prev_commit} is not a commit."
                raise QueryingError, send_error_message("#{prev_commit} is not a commit.")
            end

            file = commit.tree.access_path(file_path)

            if file.nil?
                puts "File not found in commit #{prev_commit}"
                raise QueryingError, send_error_message("File not found in commit #{prev_commit}")
            elsif file.class != GitRead::Blob
                puts "file #{tracked_path} is not a file in commit #{prev_commit}"
                raise QueryingError, end_error_message("file #{tracked_path} is not a file in commit #{prev_commit}")
            end

            # while the file didn't change, we keep digging but
            # we keep track of all the intermediates commit for
            # the interface
            if prev_file_sha == file.sha
                prev_commit = commit.parents_sha[0]
                commit_path << {
                    "commit" => commit.sha,
                    "parent_commit" => prev_commit,
                    "message" => commit.to_s
                }
            else
                commit = find_first_commit(file_path, commit_path, file.sha, commit, commit.parents_sha[0])
                keep_digging = false
            end
        end while keep_digging

        last_file = @repository.access_object(prev_file_sha)

        yield commit_path, commit, last_file, file
    end

    def load_miniature(file_req, query_string)
        query = split_query_string(query_string.to_s)
        query['path'] = file_req
        prev_commit = nil
        first_diff = nil
        begin
            yield_query(query) do |commit_path, commit, last_file, file|
                first_diff = GitRead::Diff.diff_strings(file.data, last_file.data).diff_set
                prev_commit = { 'commit' => commit.parents_sha[0].to_s,
                                'last_file' => last_file.sha.to_s,
                                'path' => query['path'] }
            end

            second_diff = nil
            file_data = nil
            yield_query(prev_commit) do |commit_path, commit, last_file, file|
                file_data = file.data
                second_diff = GitRead::Diff.diff_strings(file_data, last_file.data).diff_set
            end
        rescue QueryingError => e
            return e.message
        end

        begin
            temp_file = File.join(Dir.tmpdir, Pathname.new(query['path']).basename)
            open(temp_file, 'w') { |file| file.write(file_data) }

            temp_diff = temp_file + '.diff'
            open(temp_diff, 'w') do |file|
                second_diff.merge_with( first_diff ) { |range| file.write(range.to_s + "\n") }
            end

            generated_file = temp_file + '.png'
            
            puts "#{@codeoverview_exec} --output=#{generated_file} --diff=#{temp_diff} #{temp_file}"
            puts `#{@codeoverview_exec} --output=#{generated_file} --diff=#{temp_diff} #{temp_file}`

            system(@codeoverview_exec,
                    "--output=#{generated_file}",
                    "--diff=#{temp_diff}",
                    temp_file)
            data = open(generated_file, 'rb') { |file| file.read }

            [200, {'Content-Type' => 'image/png'}, [data]]
        rescue => e
            puts e.message
            e.backtrace.each { |v| puts v }
        end
    end

    def load_commit(deep, commit)
        commit_ref = GitRead::ShaRef.new(commit)
        commit = @repository.access_object(commit_ref)

        if commit.nil?
            puts "Commit not found #{commit}"
            raise QueryingError, send_error_message("Commit not found #{commit}")
        elsif commit.class != GitRead::Commit
            puts "#{commit} is not a commit."
            raise QueryingError, send_error_message("#{commit} is not a commit.")
        end

        prev_commit = @repository.access_object(commit.parents_sha[0])
        if prev_commit.nil?
            puts "No parent for #{commit}"
            raise QueryingError, send_error_message("Parent commit not found #{commit}")
        elsif commit.class != GitRead::Commit
            puts "#{commit} parent is not a commit."
            raise QueryingError, send_error_message("#{commit} parent is not a commit.")
        end


        if deep
            clean_message = self.cleanup_encoding(commit.message)
            rez = { :message => clean_message,
                    :parents_sha => commit.parents_sha,
                    :key => commit.sha,
                    :author => commit.author,
                    :file_changes => prev_commit.diff_with_content(commit) }
        else
            rez = prev_commit.diff(commit)
        end
        [200, { 'Content-Type' => 'text/json' }, [rez.to_json]]
    end

    def cleanup_encoding(str)
        return str if @is_1_8

        default_encoding = Encoding.default_external
        utf8 = Encoding.find("utf-8")

        encoding_conversion_param = { :invalid => :replace, :undef => :replace }
        str.force_encoding(default_encoding).encode(utf8, encoding_conversion_param)
    end

    def load_parent(file_req, query_string)
        query = split_query_string(query_string.to_s)
        query['path'] = file_req
        begin
            encoded = yield_query(query) do |commit_path, commit, last_file, file|

                diff = GitRead::Diff.diff_strings(file.data, last_file.data)

                encoding_conversion_param = { :invalid => :replace, :undef => :replace }
                clean_data = self.cleanup_encoding(file.data)
                clean_message = self.cleanup_encoding(commit.message)
                clean_path = commit_path.map do |it|
                    it['message']= self.cleanup_encoding(it['message'])
                    it
                end

                { "data" => clean_data,
                  "filekey" => file.sha,
                  "parent_commit" => commit.parents_sha[0],
                  "message" => clean_message,
                  "diff" => diff.diff_set,
                  "path" => clean_path,
                  "key" => commit.sha.to_s
                }
            end
        rescue QueryingError => e
            return e.message
        end

        response = encoded.to_json
        [200, {'Content-Type' => 'text/json'}, [response]]
    end

    def serve_not_found_error
        html_error = <<END
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
        <title>#{@tracked_path}</title>
        <link href="difftimeline.css" type="text/css" rel="stylesheet" />
    </head>
    <body onUnload="leave_server()">
        <div class="error_panel">
            File not found in repository : #{@tracked_path}
        </div>
    </body>
</html>
END
        [200, {'Content-Type' => 'text/html'}, [html_error]]
    end

    def first_line(str)
        str.lines{ |v| return v }
    end

    def serve_base_page
        current_head = @repository.head_sha
        commit = @repository.access_object(current_head)
        tree = commit.tree
        file = tree.access_path(@tracked_path.to_s)

        if file.nil?
            return serve_not_found_error()
        end

        clean_data = self.cleanup_encoding(file.data)
        clean_message = self.cleanup_encoding(commit.message)

        base_application_state = <<END
        application_state.start_file(
           { file: "#{@tracked_path}"
           , key: "#{current_head}"
           , filekey: "#{file.sha}"
           , parent_commit: "#{commit.parents_sha[0]}"
           , data: #{clean_data.to_json}
           , message: #{clean_message.to_json}
           , diff: []
           , path: []
           }
        );
END

        [200, {'Content-Type' => 'text/javascript'}, [base_application_state]]
    end
end

state = nil
begin
    state = DiffTimelineState.new()
    puts '> Found git repository'
rescue RepositoryNotFound
    puts "Error : no git repository found"
    exit(1)
end

# a bit roots and does not cover everything, but hey, should work
def html_encode_file(f)
    f.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')
end

def kind_of_filename(fname)
    case fname
    when /\.js$/
        'text/javascript'
    when /\.css$/
        'text/css'
    else
        'text/html'
    end
end

def serve_file(filename)
    ret = open(filename, 'rb') do |file|
        file.read
    end
    kind = kind_of_filename(filename.to_s)
    [200, {'Content-Type' => kind}, [ret]]
end

def first_filename(p)
    p.each_filename { |v| return v }
end

def file_rest(p)
    ret = []
    dropped = false

    p.each_filename do |v|
        if dropped
            ret << v
        else
            dropped = true
        end
    end

    ret.join("/")
end

def server_process(state, port, request, socket)
  static_files = [ 'difftimeline.css', 'difftimeline.js', 'jquery-1.6.4.min.js',
                 'ICanHaz.min.js',
                 'screen.css', 'favicon.ico', 'underscore-min.js',
                 'tinysyntaxhighlighter.js', 'syntax-highlight.css']

  begin
    requested = request[:uri][:path].to_s
    command = first_filename(Pathname.new(requested))

    if command == 'ask_parent'
        req_file = file_rest(Pathname(requested))
        $stdout.flush
        state.load_parent(req_file, request[:uri][:query])
    elsif command == 'ask_commit'
        req_commit = Pathname(requested).basename.to_s
        puts "> ask_commit #{req_commit}"
        $stdout.flush
        state.load_commit(false, req_commit)
    elsif command == 'commit'
        req_commit = Pathname(requested).basename.to_s
        puts "> commit #{req_commit}"
        $stdout.flush
        state.load_commit(true, req_commit)
    elsif command == 'miniature'
        req_file = file_rest(Pathname(requested))
        state.load_miniature(req_file, request[:uri][:query])
    elsif command == 'quit'
        puts "Leaving"
        exit 0
    elsif command == 'initial_info.js'
        puts '> Serving initial_info.js'
        $stdout.flush
        state.serve_base_page
    else
        requested_file = requested.to_s.slice(1, requested.size)
        static_index = 0
        static_index = static_files.index(requested_file)

        if static_index  != nil
            puts "> Serving file #{requested_file}\n"
            $stdout.flush
            serve_file(state.exec_path + 'static-content/' + requested_file)
        elsif requested == '/'
            puts "> Sending base page"
            $stdout.flush
            serve_file(state.exec_path + 'static-content/base_page.html')
        else
            [404, {}, []]
        end
    end
  rescue StandardError => e
      puts "Difftimeline error " + e.to_s
  end
end


def success_launcher(app_state, port)
    puts 'Ok state '
   Proc.new { app_state.launch_client(port) }
end

def launch_server(application_state, port)
    success_callback = lambda { application_state.launch_client(port) }
    begin
        puts("> Trying to launch on port #{port}")
        Net::HTTP::Server.run( :host => '127.0.0.1',
                               :port => port,
                               :success_opener => success_callback  ) do |request,socket|
            server_process(application_state, port, request, socket)
        end
    rescue Errno::EADDRINUSE
        puts "> Port #{port} already in use, trying port #{port + 1}"
        launch_server(application_state, port + 1)
    end
end

launch_server(state, 8080)
