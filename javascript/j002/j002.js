
document.write(examdatabase.description);

function question(){
	document.getElementById('question_space').innerHTML = "Question: " + examdatabase.questions[0].question;
}

function answer(){
	document.getElementById('answer_space').innerHTML = "Answer: " + examdatabase.questions[0].answer;
}


