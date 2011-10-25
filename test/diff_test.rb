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
diff.print_diff
puts diff.diff_set.to_json

# ignoring this part for now
(0..2).each do |v|
    puts "\n=====================================\n"
    puts "Diffing different in middle"
    beg_time = Time.now
    diff = GitRead::Diff.diff_files('test/huge1.cpp', 'test/huge2.cpp')
    puts "#{Time.now - beg_time}"
    diff.print_diff
    puts diff.diff_set.to_json

    #diff test/huge1.cpp test/huge2.cpp 
    #879,880d878
    #<   this->height = size;
    #<   this->width  = size;
    #1183a1182
    #>   /* blah blah */
    puts "\n=====================================\n"

    puts "Diffing different both ends"
    beg_time = Time.now
    diff = GitRead::Diff.diff_files('test/huge1.cpp', 'test/huge3.cpp')
    puts "#{Time.now - beg_time}"
    #diff test/huge1.cpp test/huge3.cpp
    #1d0
    #< #include "TextureUtils.h"
    #2336a2336
    #> /* PWET */

    diff.print_diff
    puts diff.diff_set.to_json
end
