#Ruby Task: r001
#Author: Jose Rivera
#CS 3342



puts 'Now processing....'
puts 'You can find the ouput file in the relative directory.'

fname = "r001out.txt"

#When passing parameter to .open, "w" overwrites current file while 
#"a" appends to it and "r" reads
outputfile = File.open(fname, "w")

inputs = Array.new(6)
		
input = File.open("logout.txt").each do |line|
		
		
		if line[/\AAuthor: Bob/]
			line.chomp!
		else
			outputfile.puts line
		end
	
end
outputfile.close
input.close

#TO DO: Devlop Code to group code and delete based on author
#TO DO: Develop code to ouput straight to HTML file
