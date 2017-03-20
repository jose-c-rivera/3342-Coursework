#Ruby Task: r030
#Author: Jose Rivera
#Purpose: Build a ruby interpreter for the output of r020
#Course: CS 3342

fname = "output.txt"


outputfile = File.open(fname, "w")

inputs = Array.new(6)
		
input = File.open("r020output.txt").each do |line|

		if line.include? ";"
                        string = line.strip
                        num = string.to_i
			outputfile.puts line
		end

		if line.include? ("i") or line.include? ("f")
			string = line.strip
                        num = string.to_i
			outputfile.puts "expr \n [ \n term \n [ \n factor \n [ \n integer_constant \n [ "
			outputfile.puts line
			outputfile.puts " ]\n ]\n ]\n ]\n ]"
		end


	
end
puts "The file can be found in the local directory."
outputfile.close
input.close

#Add symbols of the grammer
#Recognize integer/float
