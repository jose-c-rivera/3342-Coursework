require 'json'
# https://hackhands.com/ruby-read-json-file-hash/
# note: if you don't have json, you need to do: gem install json
# however, since json is part of stdlib, you really should have it already
# http://ruby-doc.org/stdlib-2.4.0/libdoc/json/rdoc/JSON.html

class ExamDatabase
  attr_reader :file_size
  def initialize(exam_database_file)
    file_contents = File.read(exam_database_file)
    @file_size = file_contents.size
    @database = JSON.parse(file_contents)
  end
  def dump
    dump_array(@database["questions"])
  end
  private def dump_array(array)
    array.each { |question| dump_question(question) }
  end
  private def dump_question(question)
    return  if question.nil?
    puts "question is: " + question["question"]
    dump_answer(question["answer"])
  end
  private def dump_answer(answer)
    if answer.instance_of? String then
      puts "answer is: " + answer
    elsif answer.instance_of? Array then
      answer.each { |answer| dump_answer(answer) }
    else 
      puts "UNKNOWN ANSWER FORMAT"
    end
  end
end

class ExamConfig
  attr_reader :exam_database_file, :exam_format, :dump_database
  def initialize(arg_list)
    if arg_list.any? { |value| /^database=/ =~ value } then
      database_arg = arg_list.find { |value| /^database=/ =~ value }
      @exam_database_file = database_arg.sub("database=","")
    else
      @exam_database_file = "examdatabase.json"
    end
    if arg_list.include?("latex") then
      @exam_format = "latex"
    else
      @exam_format = "plain"
    end
    if arg_list.include?("dumpdatabase") then
      @dump_database = true
    else
      @dump_database = false
    end
    if arg_list.include?("dump") then
      dump
    end
  end
  def dump
    puts("exam_database_file= " + @exam_database_file)
    puts("exam_format= " + @exam_format)
  end
end

config = ExamConfig.new(ARGV)
exam_database = ExamDatabase.new(config.exam_database_file)
if config.dump_database then
  exam_database.dump
end
