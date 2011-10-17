require 'pp'

unless Kernel.respond_to?(:require_relative)
  module Kernel
    def require_relative(path)
      require File.join(File.dirname(caller[0]), path.to_str)
    end
  end
end

require_relative '../lib/diff.rb'

diff = GitRead::Diff.diff_files('test/file1_orig.txt', 'test/file1_dest.txt')
puts "\n=====================================\n"
diff.print_diff(false)
pp diff.diff_set

(0..2).each do |v|
    puts "\n=====================================\n"
    puts "Diffing different in middle"
    beg_time = Time.now
    diff = GitRead::Diff.diff_files('test/huge1.cpp', 'test/huge2.cpp')
    diff.print_diff(false)
    puts "#{Time.now - beg_time}"

    puts "\n=====================================\n"

    puts "Diffing different both ends"
    beg_time = Time.now
    diff = GitRead::Diff.diff_files('test/huge1.cpp', 'test/huge3.cpp')
    diff.print_diff(false)
    puts "#{Time.now - beg_time}"
end
