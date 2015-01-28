var socket = new WebSocket("ws://" + window.location.host + "/socket");
var connected = false;

socket.onopen = function()
{
	connected = true;
}

socket.onerror = function(error)
{
	alert("WebSocket error: " + error);
}

socket.onmessage = function(msg)
{
	var cont = document.getElementById("chatbox-content")
	cont.innerHTML = cont.innerHTML + "<br />" + msg.data;
}

function sendMessage()
{
	var input = document.getElementById("inputbox-content");
	var message = input.value;
	if (connected)
	{
		socket.send(message);
	}
	input.value = "";
}
