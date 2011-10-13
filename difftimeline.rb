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

def serve_css
    ret = open('difftimeline.css', 'rb') do |file|
        file.read
    end
    [200, {'Content-Type' => 'text/css'}, [ret]]
end

def serve_base_page(repository, current_head, tracked_path)
  commit = repository.access_object(current_head)
  tree = commit.tree
  file = tree.access_path(tracked_path)
  encoded_data = file.data.gsub('&', '&amp;').gsub('<', '&lt;').gsub('>', '&gt;')

  html_doc = <<END
    <html>
        <head>
            <title>#{tracked_path}</title>
            <link href="difftimeline.css" type="text/css" rel="stylesheet" />
        </head>
        <body>
            <div class="returnpast">
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

Net::HTTP::Server.run(:host => '127.0.0.1', :port => 8080) do |request,socket|
  pp request

  case request[:uri][:path]
  when '/difftimeline.css'
      serve_css()
  when '/'
      serve_base_page(repository, current_head, tracked_path)
  end
end

