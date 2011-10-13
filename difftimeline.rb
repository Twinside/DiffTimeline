#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
require 'rubygems'
require 'net/http/server'
require 'launchy'
require 'pp'
require 'pathname'

require_relative 'lib/objects'
require_relative 'lib/repository'

if ARGV.size <= 0
    puts "Error: no specified file"
    puts "   syntax : ruby diffTimeline.rb file"
    exit(1)
end

exec_path = Pathname(__FILE__).realpath.parent

def find_nearest_git_repo
    currDir = Dir.pwd   
    Pathname.new(Dir.pwd).ascend do |v|
        gitDir = v + '.git'
        return v if File.exists?(gitDir)
    end
end

current_dir = Pathname.new(Dir.pwd)
repo_dir = find_nearest_git_repo()
tracked_path = (current_dir + Pathname.new(ARGV[0])).cleanpath.relative_path_from(Pathname.new(repo_dir))
repository = GitRead::Repository.new(repo_dir)

current_head = repository.head_sha

# Serious race condition, but it will be ok to test
Launchy.open('http://127.0.0.1:8080')

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

def serve_base_page(repository, current_head, tracked_path)
  commit = repository.access_object(current_head)
  tree = commit.tree
  file = tree.access_path(tracked_path.to_s)
  encoded_data = file.data.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')

  html_doc = <<END
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
        <title>#{tracked_path}</title>
        <link href="difftimeline.css" type="text/css" rel="stylesheet" />
        <!-- Loading from local path to be able to work offline -->
        <script language="javascript" type="text/javascript" src="jquery-1.6.4.min.js"></script>
        <script language="javascript" type="text/javascript" src="difftimeline.js"></script>
        <script language="javascript" type="text/javascript">
            var last_infos = { file: "#{tracked_path}", key: "#{current_head}"
                             , parent_commit: "#{commit.parents_sha[0]}" };
        </script>
    </head>
    <body onUnload="leave_server()">
        <div class="returnpast" onClick="back_to_the_past()">
            &lt;&lt;
        </div>
        <div class="commit">
            <div class="commitmsg">#{commit}</div>
            <div class="file_content">
                <pre>#{encoded_data}</pre>
            </div>
        </div>
    </body>
</html>
END

  [200, {'Content-Type' => 'text/html'}, [html_doc]]
end

def load_parent(repository, current_head, tracked_path, query_string)
    pp "Asking #{query_string}"
    [200, {'Content-Type' => 'text/html'}, ['[3]']]
end

Net::HTTP::Server.run(:host => '127.0.0.1', :port => 8080) do |request,socket|
  requested = request[:uri][:path]
  if requested == '/ask_parent'
      load_parent(repository, current_head, tracked_path, request[:uri][:query])
  elsif requested == '/quit'
      puts "Leaving"
      exit 0
  elsif File.exists?('.' + requested) && requested != '/'
      serve_file(exec_path + requested.slice(1, requested.size))
  else
      serve_base_page(repository, current_head, tracked_path)
  end
end

