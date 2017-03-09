#Ruby Task: r001
#Author: Jose Rivera
#CS 3342


require 'open3'
require 'rubygems'
require 'pp'
logs = STDIN.read
logs = logs.split("commit ")
logs.shift

logs = logs.map do |log|
  l = log.split("\n")
  commit = l.shift
  author = l.shift.to_s.split("Author: ")[1]
  x = author.to_s.split(" ")
  email = x.pop
  name = x.join(' ')

  date = l.shift.to_s.split("Date : "[1].to_s.strip
  comments = l.join("\n")
  {
    :commit => commit,
    :author => author,
    :name => name,
    :email => email,
    :date => date,
    :comments => comments
  }
end
pp logs


puts 'Now processing....'
puts 'You can find the ouput file in the relative directory.'

fname = "r001out.txt"

#When passing parameter to .open, "w" overwrites current file while 
#"a" appends to it and "r" reads
outputfile = File.open(fname, "w")

inputs = Array.new(6)

input = Open3.capture3("git log").each do |line|
				
		if line.include? ("Bob") or line.include? ("practice framework")
			line.chomp!
		else
			outputfile.puts line
		end
	
end
outputfile.close
input.close

#TO DO: Devlop Code to group code and delete based on author
#TO DO: Develop code to ouput straight to HTML file
