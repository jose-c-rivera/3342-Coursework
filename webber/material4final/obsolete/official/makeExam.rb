require 'json'
# https://hackhands.com/ruby-read-json-file-hash/
# note: if you don't have json, you need to do: gem install json
# however, since json is part of stdlib, you really should have it already
# http://ruby-doc.org/stdlib-2.4.0/libdoc/json/rdoc/JSON.html

class NilClass
  def size
    0
  end
end

class Array
  def rest
    self[1..-1]
  end
  def second
    self[1]
  end
end

class String
  def multiline(width)
    array = self.split
    rebuild array, width
  end
  private def rebuild(array, width)
    if array.size === 0 then
      []
    elsif array.size === 1 then
      array
    elsif array.first.size <= width then
      if (array.first + array.second).size <= width then
        rebuild ([array.first + " " + array.second] + array.rest.rest), width
      else
        [array.first] + (rebuild array.rest, width)
      end
    else
      [ array.first ] + (rebuild array.rest, width)
    end
  end
end

class ExamDatabase
  attr_reader :items
  def initialize(exam_database_file)
    file_contents = File.read exam_database_file
    @file_size = file_contents.size
    @database = JSON.parse file_contents
    @items = extract @database
  end
  def dump(formatter)
    dump_array @items, formatter
  end
  private def extract(database)
    items = @database["questions"].compact
    @items = items.map { | item | Item.new(item) }
  end
  def dump_array(array, formatter, filter=:both)
    count = 0
    array.each do |item| 
                  count = count + 1
                  item.dump count, formatter, filter
               end
  end
end

class Chapters
  attr_reader :chapters, :distribution
  def initialize(items)
    @chapters = (items.map { | item | item.source }).uniq
    @distribution = build_distribution(items)
    @chapterize = build_chapterization(items)
  end
  def size
    result = 0
    @chapters.map { | chapter | result += @chapterize[chapter].size }
    result
  end
  def sample_round prng
    remaining_items = Array.new
    items_selected = Array.new
    @chapters.map do | chapter |
      current_items = @chapterize[chapter]
      current_sample = current_items.sample random: prng
      items_selected = items_selected << current_sample
      current_items = current_items.select { | item | item != current_sample }
      remaining_items = remaining_items.concat(current_items)
    end
    return remaining_items, items_selected
  end
  private def build_distribution(items)
    distribution = Hash.new
    @chapters.map { | chapter | distribution[chapter] = 0 }
    items.map { | item | distribution[item.source] += 1 }
    distribution
  end
  private def build_chapterization(items)
    chapterize = Hash.new
    @chapters.map { | chapter | chapterize[chapter] = Array.new }    
    items.map do | item | 
      source = item.source
      chapterize[source] = chapterize[source] << item
    end
    chapterize
  end
end

class Item
  attr_reader :question, :lquestion, :answer, :lanswer, :source
  def initialize(item_parts)
    @question = item_parts["question"]
    if item_parts.key? "lquestion" then
      @lquestion = item_parts["lquestion"]
    else
      @lquestion = @question
    end
    @answer = item_parts["answer"]
    if item_parts.key? "lanswer" then
      @lanswer = item_parts["lanswer"]
    else
      @lanswer = @answer
    end
    @source = item_parts["source"]
  end
  def dump(count, formatter, filter)
    formatter.skip_line
    if filter === :both then
      formatter.dump_both self, filter, count
    elsif filter == :question then 
      formatter.dump_question self, filter, count
    elsif filter == :answer then
      formatter.dump_answer self, filter, count
    else 
      puts "Unknown filter: " + filter.to_s
    end          
  end
end

class PlainFormatter
  def initialize(width)
    @width = width
  end
  def prologue
    puts <<-END_OF_STRING
FINAL EXAM: CS3342b Tuesday, 25 April 2017, 2pm, Room FEB GYM

NAME AS APPEARS ON STUDENT ID:

STUDENT ID NUMBER:

GAUL/CONFLUENCE USER NAME:

REMINDERS:
** (from course outline) The final exam will be closed book, 
   closed notes, with no electronic devices allowed, with particular 
   reference to any electronic devices that are capable of communication 
   and/or storing information.

** Write neatly.  If the marker can't read it, it is wrong.

** This exam shouldn't take long to write.  On the other hand, time
   will pass.  It is a 3 hour exam with 50 questions.  If you complete
   a question every 3 minutes (or 10 questions every half hour), you
   will still have a half hour at the end to double check that
   everything is in order.

** While you are not allowed to open the exam booklet until the
   proctor says you can, you can fill out the information on the cover
   page.  You should also get out your student id and make sure your
   pencils and pens are in order.  If you need to get something out of
   your jacket or knapsack once the exam has started, raise your hand
   and wait til a proctor comes to you to oversee the matter.
            END_OF_STRING
  end
  def epilogue
  end
  def page_break
    puts "\f"
  end
  def skip_line
    puts ""
  end
  def dump_both(item, filter, count)
      question = ("QUESTION " + count.to_s + ": " + item.question).multiline(@width)
      question.each { | line | puts line }
      dump_answer item, filter, count
  end
  def dump_question(item, filter, count)
      question = ("QUESTION " + count.to_s + ": " + item.question).multiline(@width)
      question.each { | line | puts line }
      puts "ANSWER="
  end
  def dump_answer(item, filter, count)
    if item.answer.instance_of? String then
      dump_answer_string item.answer, filter, count      
    elsif item.answer.instance_of? Array then
      item.answer.each { |answer| dump_answer_string answer, filter, count }
    else 
      puts "UNKNOWN ANSWER FORMAT"
    end
  end
  def dump_answer_string(answer, filter, count)
    if filter === :answer then
      answer_prefix = "ANSWER " + count.to_s + "= "
    else
      answer_prefix = "ANSWER= "
    end
      puts answer_prefix + answer
  end
  def dump_configuration(config, items, possible_exam)
    config.dump
    chapters = Chapters.new(items).chapters.sort.to_s.multiline(@width)
    chapters.each { | line | puts line }
    chapters = Chapters.new(possible_exam).chapters.sort.to_s.multiline(@width)
    chapters.each { | line | puts line }
    puts "---------------"
  end
end

class LatexFormatter
  def initialize
    @width = 72
  end
  def prologue
    puts <<-END_OF_STRING
\\documentclass{exam}
\\begin{document}
FINAL EXAM: CS3342b Tuesday, 25 April 2017, 2pm, Room FEB GYM\\newline
\\newline
\\newline
\\newline
NAME AS APPEARS ON STUDENT ID:\\newline
\\newline
STUDENT ID NUMBER:\\newline
\\newline
GAUL/CONFLUENCE USER NAME:\\newline
\\newline
REMINDERS:
\\begin{enumerate}
\\item (from course outline) The final exam will be closed book, closed notes, with no electronic devices allowed, with particular reference to any electronic devices that are capable of communication and/or storing information.
\\item Write neatly.  If the marker can't read it, it is wrong.
\\item This exam shouldn't take long to write.  On the other hand, time will pass.  It is a 3 hour exam with 50 questions.  If you complete a question every 3 minutes (or 10 questions every half hour), you will still have a half hour at the end to double check that everything is in order.
\\item While you are not allowed to open the exam booklet until the proctor says you can, you can fill out the information on the cover page.  You should also get out your student id and make sure your pencils and pens are in order.  If you need to get something out of your jacket or knapsack once the exam has started, raise your hand and wait til a proctor comes to you to oversee the matter.
\\end{enumerate}
\\newpage
\\begin{enumerate}
            END_OF_STRING
  end
  def epilogue
    puts <<-END_OF_STRING
\\end{enumerate}
\\end{document}
            END_OF_STRING
  end
  def page_break
    puts "\\end{enumerate}"
    puts "\\newpage"
  end
  def skip_line
  end
  def dump_both(item, filter, count)
      question = "\\item " + item.lquestion
      puts question
      puts "\\begin{itemize}"
      dump_answer item, filter, count
      puts "\\end{itemize}"
  end
  def dump_question(item, filter, count)
      question = "\\item " + item.lquestion
      puts question + "\\newline"
      puts "ANSWER="
  end
  def dump_answer(item, filter, count)
    if item.lanswer.instance_of? String then
      dump_answer_string item.lanswer, filter, count      
    elsif item.answer.instance_of? Array then
     puts "\\item \\begin{itemize}" if filter != :both
     item.lanswer.each { |answer| dump_answer_string answer, filter, count }
     puts "\\end{itemize}" if filter != :both
    else 
      puts "UNKNOWN ANSWER FORMAT"
    end
  end
  def dump_answer_string(answer, filter, count)
    if filter === :answer then
      answer_prefix = "\\item "
    else
      answer_prefix = "\\item "
    end
    puts answer_prefix + answer
  end
  def dump_configuration(config, items, possible_exam)
    puts "\\begin{verbatim}"
    config.dump
    chapters = Chapters.new(items).chapters.sort.to_s.multiline(@width)
    chapters.each { | line | puts line }
    chapters = Chapters.new(possible_exam).chapters.sort.to_s.multiline(@width)
    chapters.each { | line | puts line }
    puts "\\end{verbatim}"
    puts "\\begin{enumerate}"
  end
end

class ExamConfig
  attr_reader :exam_database_file, :exam_format, :dump_database, 
              :question_count, :create_exam, :answer_key, :sample_prng, 
              :shuffle_prng, :exam_formatter
  def initialize(arg_list)
    if arg_list.any? { |value| /^database=/ =~ value } then
      database_arg = arg_list.find { |value| /^database=/ =~ value }
      @exam_database_file = database_arg.sub "database=", ""
    else
      @exam_database_file = "examdatabase.json"
    end
    if arg_list.any? { |value| /^width=/ =~ value } then
      width_arg = arg_list.find { |value| /^width=/ =~ value }
      @line_width = (width_arg.sub "width=", "").to_i
    else
      # original line width limit for American teletypewriters
      # https://en.wikipedia.org/wiki/Characters_per_line
      @line_width = 72 
    end
    if arg_list.any? { |value| /^question_count=/ =~ value } then
      question_count_arg = arg_list.find { |value| /^question_count=/ =~ value }
      @question_count = (question_count_arg.sub "question_count=", "").to_i
    else
      @question_count = 50
    end
    if arg_list.any? { |value| /^shuffle_seed=/ =~ value } then
      shuffle_seed_arg = arg_list.find { |value| /^shuffle_seed=/ =~ value }
      @shuffle_seed = (shuffle_seed_arg.sub "shuffle_seed=", "").to_i
      @shuffle_prng = Random.new @shuffle_seed
    else
      @shuffle_seed = 654
      @shuffle_prng = Random.new @shuffle_seed
    end
    if arg_list.any? { |value| /^sample_seed=/ =~ value } then
      sample_seed_arg = arg_list.find { |value| /^sample_seed=/ =~ value }
      @sample_seed = (sample_seed_arg.sub "sample_seed=", "").to_i
      @sample_prng = Random.new @sample_seed
    else
      @sample_seed = 6504
      @sample_prng = Random.new  @sample_seed
    end
    if arg_list.include?("surprise_me") then
      @sample_seed = "unknown"
      @shuffle_seed = "unknown"
      @shuffle_prng = Random.new
      @sample_prng = Random.new
    end
    if arg_list.include?("latex") then
      @exam_format = "latex"
      @exam_formatter = LatexFormatter.new
    else
      @exam_format = "plain"
      @exam_formatter = PlainFormatter.new @line_width
    end
    if arg_list.include?("dump_database") then
      @dump_database = true
    else
      @dump_database = false
    end
    if arg_list.include?("create_exam") then
      @create_exam = true
    else
      @create_exam = false
    end
    if arg_list.include?("answer_key") then
      @answer_key = true
    else
      @answer_key = false
    end
    if arg_list.include?("dump") then
      dump
    end
  end
  def dump
    puts "exam_database_file= " + @exam_database_file
    puts "exam_format= " + @exam_format
    puts "dump_database= " + @dump_database.to_s
    puts "line_width= " + @line_width.to_s
    puts "question_count= " + @question_count.to_s
    puts "create_exam= " + @create_exam.to_s
    puts "answer_key= " + @answer_key.to_s
    puts "sample_seed= " + @sample_seed.to_s
    puts "shuffle_seed= " + @shuffle_seed.to_s
  end
end

def print_chapter_distribution(items)
  the_chapters = Chapters.new items
  the_chapters.chapters.sort.each do | chapter |
     puts chapter + " : " + the_chapters.distribution[chapter].to_s
  end
end

def build_exam(items, config)
  exam = Array.new
  count = config.question_count

  chapters = Chapters.new(items)
  if count >= chapters.chapters.size then
    items, round1 = chapters.sample_round config.sample_prng
    count = count - round1.size
    exam = exam.concat round1
  end

  chapters = Chapters.new(items)
  if count >= chapters.chapters.size then
    items, round2 = chapters.sample_round config.sample_prng
    count = count - round2.size
    exam = exam.concat round2
  end

  exam = exam.concat items.sample(count, random: config.sample_prng)
  exam = exam.shuffle(random: config.shuffle_prng)
  exam = exam.shuffle(random: config.shuffle_prng)
  exam = exam.shuffle(random: config.shuffle_prng)
end


config = ExamConfig.new ARGV
exam_database = ExamDatabase.new config.exam_database_file
if config.dump_database then
  config.exam_formatter.prologue
  exam_database.dump config.exam_formatter
  config.exam_formatter.epilogue
end
if config.create_exam
  config.exam_formatter.prologue
  possible_exam = build_exam exam_database.items, config
  exam_database.dump_array possible_exam, config.exam_formatter
  config.exam_formatter.epilogue
end
if config.answer_key
  config.exam_formatter.prologue
  possible_exam = build_exam exam_database.items, config
  exam_database.dump_array possible_exam, config.exam_formatter, :question
  config.exam_formatter.page_break
  config.exam_formatter.dump_configuration config, exam_database.items, possible_exam
  exam_database.dump_array possible_exam, config.exam_formatter, :answer
  config.exam_formatter.epilogue
end
