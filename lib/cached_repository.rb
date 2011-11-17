require_relative 'repository'
require 'pp'

module GitRead
    class CachedRepository < Repository
        def initialize(root_path)
            super(root_path)
            @cache = {}
            @cached = 0
            @fetched = 0
        end

        def access_object(sha)
            cached = @cache[sha.to_s]
            if cached != nil
                @cached += 1
                return cached 
            end
            @fetched += 1
            cached = super(sha)
            @cache[sha.to_s] = cached
            cached
        end
    end
end

