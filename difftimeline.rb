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

Net::HTTP::Server.run(:host => '127.0.0.1', :port => 8080) do |request,socket|
  pp request

  commit = repository.access_object(current_head)
  tree = repository.access_object(commit.tree)
  file = tree.access_path(repository, tracked_path)

  ret = "<span class=\"commitmsg\">#{commit}</span><pre>#{file.data}</pre>"

  htmlDoc = "<html><head><title>#{tracked_path}</title></head><body>" + ret + '</body></html>'
  [200, {'Content-Type' => 'text/html'}, [htmlDoc]]
end

