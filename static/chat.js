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

socket.onmessage = function(packet)
{
	var cont = document.getElementById("chatbox-content");
	var split = packet.data.split(" ");

	var scroll = cont.scrollTop >= (cont.scrollHeight - cont.offsetHeight);

	var action = split[0];
	if (action == "MSG")
	{
		var nick = split[1];
		var msg = split[2];

		if (nick != undefined && msg != undefined)
		{
			cont.innerHTML = cont.innerHTML + "&lt;" + nick + "&gt; " + msg + "<br />";
		}

		if (scroll)
		{
			cont.scrollTop = cont.scrollHeight;
		}
	}
	else if (action == "NICK")
	{
		var oldNick = split[1];
		var newNick = split[2];

		if (oldNick != undefined && newNick != undefined)
		{
			cont.innerHTML = cont.innerHTML + "* " + oldNick + " is now known as " + newNick + "<br />";
		}

		if (scroll)
		{
			cont.scrollTop = cont.scrollHeight;
		}
	}
	else if (action == "QUIT")
	{
		var nick = split[1];

		if (nick != undefined)
		{
			cont.innerHTML = cont.innerHTML + "* " + nick + " has quit<br />";
		}

		if (scroll)
		{
			cont.scrollTop = cont.scrollHeight;
		}
	}
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
