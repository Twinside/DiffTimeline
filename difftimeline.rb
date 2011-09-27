#require 'dir'
require 'rubygems'
require 'net/http/server'
require 'launchy'
require 'pp'
require 'zlib'

require_relative 'lib/packfile'
require_relative 'lib/objects'

def unpackPackIndex(filename)
    open(filename, 'rb') do |file|
        #unpacked = Zlib::Inflate.inflate(file.read)
        BinData::trace_reading do
            return GitRead::PackFileIndex.read(file)
        end
    end
end

def unpackPackFile(filename)
    open(filename,'rb') do |file|
        BinData::trace_reading do
            return GitRead::PackFile.read(file)
        end
    end
end
# Serious race condition, but it will be ok to test
#Launchy.open('http://127.0.0.1:8080')

#repository = Git.open (Dir.pwd)
#repository = Git.open (Dir.pwd + '/.git', :log => Logger.new(STDOUT))
#pp unpackPackIndex('C:/Users/Vince/Desktop/Webrexp/.git/objects/pack/pack-87211975add2b739089b468a6447f5b37efe3ae4.idx')
#pp unpackPackFile('C:/Users/Vince/Desktop/Webrexp/.git/objects/pack/pack-87211975add2b739089b468a6447f5b37efe3ae4.pack')

def unpackBlob(path)
    pp GitRead::readObject(GitRead::unpackBlob(path))
end

unpackBlob('.git/objects/21/35b9946b354a3ca4c8295be04f14a7632e9871')
unpackBlob('.git/objects/08/71a4f6ce5c94709897b32e54040df8870292b7')
unpackBlob('.git/objects/24/7d5e1bb82d838293ca57be13b211da8071e2ec')
unpackBlob('.git/objects/30/1b7bf57ae4ec219aee3851148353fb090acb8b')
unpackBlob('.git/objects/58/1539e8ffd5772ad66736b45adfae8f964c24b4')
unpackBlob('.git/objects/70/4af163b4acfc999a02677cc004879547da6a7b')
unpackBlob('.git/objects/80/972067a10f778dd1161cf6506548b2e40b7799')
unpackBlob('.git/objects/a1/a5c9d0eef192a441f29d0c85f35e7655bdae7d')
unpackBlob('.git/objects/b6/7578dcffa93cd2b9e1f85c4403073bdc76988e')
unpackBlob('.git/objects/ba/7f26fa91f81a49cfc4d36f7125d2ca7253600f')
unpackBlob('.git/objects/d7/d861f88f95765a59c738cf0360310a9833ef01')
unpackBlob('.git/objects/e7/89338df41d819402e5eeb19be6adb5c4ee7780')
unpackBlob('.git/objects/0f/8dfce46d4106fa649a4ecfe2e193e484bfb845')

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

