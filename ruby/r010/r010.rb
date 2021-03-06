#Ruby Task: r010
#Author: Jose Rivera
#Purpose: Build a scanner in Ruby to read from an input file
#Course: CS 3342


fname = "output.txt"


outputfile = File.open(fname, "w")

inputs = Array.new(6)
		
input = File.open("input.txt").each_char do |char|

		if char[/\d/]
                        string = char.strip
                        num = string.to_i
			outputfile.puts "i  #{num}"
		else
			outputfile.puts char
		end
	
end
outputfile.close
input.close

#Notes to consider:
#Right now the program works if the numbers are on seperate lines
#Also the program must be able to distinguish integers and floats
#lexical syntax
