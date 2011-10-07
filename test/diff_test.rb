require 'pp'
require_relative '../lib/diff.rb'

diff = GitRead::Diff.diffFiles('test/file1_orig.txt', 'test/file1_dest.txt')
diff.dump
puts "\n=====================================\n"
diff.print_diff

pp diff.diff_set
