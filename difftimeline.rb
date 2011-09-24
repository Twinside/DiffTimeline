require 'net/http/server'
require 'launchy'
require 'pp'
#require 'dir'
require 'rubygems'
require 'git'

# Serious race condition, but it will be ok to test
Launchy.open('http://127.0.0.1:8080')

repository = Git.open (Dir.pwd)
#repository = Git.open (Dir.pwd + '/.git', :log => Logger.new(STDOUT))

branchList = repository.branches.map do |branch|
    '<li>' + branch.name + '</li>'
end

Net::HTTP::Server.run(:host => '127.0.0.1', :port => 8080) do |request,socket|
  pp request

  ret = '<ul>' + branchList.join() + '</ul>'
  htmlDoc = '<html><head><title>t</title></head><body>' + ret + '</body></html>'
  [200, {'Content-Type' => 'text/html'}, [htmlDoc]]
end

