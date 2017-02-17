var question = 0;

document.write(examdatabase.description);

function question(question){
	document.getElementById('question_space').innerHTML = "Question: " + examdatabase.questions[i].question;
}

function answer(question){
	document.getElementById('answer_space').innerHTML = "Answer: " + examdatabase.questions[i].answer;
}

function correct(){
	var textBox = document.getElementById("correct_box");
     	textBox.value++;
}
	
function incorrect(){
	var textBox = document.getElementById('incorrect_box');
	textBox.value++;
}

function undo(){
	var textBox = document.getElementById('incorrect_box');
	if(textBox.value != 0)	
		textBox.value--;
}

function newQ(){
	question ++;
}
