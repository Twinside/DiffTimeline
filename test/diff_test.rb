require 'pp'
require_relative '../lib/diff.rb'

diff = GitRead::Diff.diff_files('test/file1_orig.txt', 'test/file1_dest.txt')
diff.dump
puts "\n=====================================\n"
diff.print_diff

puts diff.diff_set.to_json
