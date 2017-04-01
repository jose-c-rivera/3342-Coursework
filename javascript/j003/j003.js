var qnum = 0;

document.write(examdatabase.description);

function question(){
	document.getElementById('question_space').innerHTML = "Question #" + qnum + ": "  + examdatabase.questions[qnum].question;
}

function answer(){
	document.getElementById('answer_space').innerHTML = "Answer #" + qnum + ": " + examdatabase.questions[qnum].answer;
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
	if(qnum<34)
	   qnum = qnum + 1;
	
	else
	   qnum=0;
}
