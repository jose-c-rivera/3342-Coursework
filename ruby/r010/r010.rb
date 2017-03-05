#Ruby Task: r010
#Author: Jose Rivera
#Purpose: Build a sacanner in Ruby to read from an input file
#Course: CS 3342


fname = "output.txt"


outputfile = File.open(fname, "w")

inputs = Array.new(6)
		
input = File.open("input.txt").each do |line|

		if line[/\d/]
                        string = line.strip
                        num = string.to_i
			outputfile.puts "i  #{num}"
		else
			outputfile.puts line
		end
	
end
outputfile.close
input.close

#Notes to consider:
#Right now the program works if the numbers are on seperate lines
#Also the program must be able to distinguish integers and floats
#lexical syntax
