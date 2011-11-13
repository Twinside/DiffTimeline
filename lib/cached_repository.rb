require_relative 'repository'

module GitRead
    class CachedRepository < Repository
        def initialize(root_path)
            super(root_path)
            @cache = {}
        end

        def access_object(sha)
            cached = @cache[sha]
            return cached if cached != nil
            puts "! Fetching #{sha}"
            cached = super(sha)
            @cache[sha] = cached
            cached
        end
    end
end

