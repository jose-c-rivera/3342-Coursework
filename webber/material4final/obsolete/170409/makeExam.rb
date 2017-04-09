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
  def dump(width)
    dump_array @items, width
  end
  private def extract(database)
    items = @database["questions"].compact
    @items = items.map { | item | Item.new(item) }
  end
  def dump_array(array, width, filter=:both)
    count = 0
    array.each do |item| 
                  count = count + 1
                  item.dump count, width, filter
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
  attr_reader :question, :answer, :source
  def initialize(item_parts)
    @question = item_parts["question"]
    @answer = item_parts["answer"]
    @source = item_parts["source"]
  end
  def dump(count, width, filter)
    puts ""
    if filter === :both then
      question = ("QUESTION " + count.to_s + ": " + @question).multiline(width)
      question.each { | line | puts line }
      dump_answer @answer
    elsif filter == :question then 
      question = ("QUESTION " + count.to_s + ": " + @question).multiline(width)
      question.each { | line | puts line }
      puts "ANSWER="
    elsif filter == :answer then
      dump_answer @answer, filter, count
    else 
      puts "Unknown filter: " + filter.to_s
    end          
  end
  private def dump_answer(answer, filter=:both, count=0)
    if filter === :answer then
      answer_prefix = "ANSWER " + count.to_s + "= "
    else
      answer_prefix = "ANSWER= "
    end
    if answer.instance_of? String then
      puts answer_prefix + answer
    elsif answer.instance_of? Array then
      answer.each { |answer| dump_answer answer, filter, count }
    else 
      puts "UNKNOWN ANSWER FORMAT"
    end
  end
end

class ExamConfig
  attr_reader :exam_database_file, :exam_format, :dump_database, :line_width,
              :question_count, :create_exam, :answer_key, :sample_prng, 
              :shuffle_prng, :sample_seed, :shuffle_seed
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
      @shuffle_seed = (question_count_arg.sub "shuffle_seed=", "").to_i
      @shuffle_prng = Random.new @shuffle_seed
    else
      @shuffle_seed = 654
      @shuffle_prng = Random.new @shuffle_seed
    end
    if arg_list.any? { |value| /^sample_seed=/ =~ value } then
      sample_seed_arg = arg_list.find { |value| /^sample_seed=/ =~ value }
      @sample_seed = (question_count_arg.sub "sample_seed=", "").to_i
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
    else
      @exam_format = "plain"
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
  exam_database.dump config.line_width
end
if config.create_exam
  possible_exam = build_exam exam_database.items, config
  exam_database.dump_array possible_exam, config.line_width
end
if config.answer_key
  possible_exam = build_exam exam_database.items, config
  exam_database.dump_array possible_exam, config.line_width, :question
  puts "\f"
  config.dump
  puts Chapters.new(exam_database.items).chapters.sort.to_s
  puts Chapters.new(possible_exam).chapters.sort.to_s
  puts "---------------"
  exam_database.dump_array possible_exam, config.line_width, :answer
end
