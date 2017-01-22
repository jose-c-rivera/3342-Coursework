
document.write(examdatabase.description);

function question(){
	document.getElementById('question_space').innerHTML = "Question: " + examdatabase.questions[0].question;
}

function answer(){
	document.getElementById('answer_space').innerHTML = "Answer: " + examdatabase.questions[0].answer;
}

function correct(){
	 var textBox = document.getElementById("correct_box");
     textBox.value++;
}
	
function incorrect(){
	var textBox = document.getElementById('incorrect_box');
	textBox.value++;
}
