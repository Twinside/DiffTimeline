require 'rubygems'
require 'net/http/server'
require 'launchy'
require 'pp'

require_relative 'lib/packfile'
require_relative 'lib/objects'
require_relative 'lib/repository'

def unpackPackIndex(filename)
    open(filename, 'rb') do |file|
        #unpacked = Zlib::Inflate.inflate(file.read)
        #BinData::trace_reading do
            #return GitRead::PackFileIndex.read(file)
        #end
        GitRead::PackFileIndex.read(file)
    end
end
pp unpackPackIndex('C:/Users/Vince/Desktop/Webrexp/.git/objects/pack/pack-87211975add2b739089b468a6447f5b37efe3ae4.idx')

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

