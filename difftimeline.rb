#!/usr/bin/env ruby
# -*- encoding: utf-8 -*-
require 'rubygems'
require 'net/http/server'
require 'launchy'
require 'pp'

require_relative 'lib/packfile'
require_relative 'lib/objects'
require_relative 'lib/repository'

repository = GitRead::Repository.new('/Users/vince/Documents/Coding/Webrexp')
head = repository.head_sha

#current = repository.access_object(head)

#while current.class == GitRead::Commit && current.parents.size > 0
    #puts current.to_s
    #current = repository.access_object( current.parents[0] )
#end
#puts current.to_s

current = repository.access_object(head)
pp current
tree = repository.access_object(current.tree)
pp tree
puts tree.access_path(repository, 'Text/Webrexp/IOMock.hs').data

# Serious race condition, but it will be ok to test
#Launchy.open('http://127.0.0.1:8080')

#branchList = repository.branches.map do |branch|
    #'<li>' + branch.name + '</li>'
#end
#myDiff = repository.diff('HEAD', 'HEAD~2')
#myDiff.each do |file|
    #pp file
#end

#Net::HTTP::Server.run(:host => '127.0.0.1', :port => 8080) do |request,socket|
  #pp request

  #ret = '<ul>' + branchList.join() + '</ul>'
  #htmlDoc = '<html><head><title>t</title></head><body>' + ret + '</body></html>'
  #[200, {'Content-Type' => 'text/html'}, [htmlDoc]]
#end

