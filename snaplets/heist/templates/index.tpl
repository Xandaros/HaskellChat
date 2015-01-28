<apply template="base">

<div id="chatbox">
	<div class="border" id="chatbox-border">
		<div id="chatbox-content">
		</div>
	</div>
</div>
<div id="inputbox">
	<div class="border" id="inputbox-border">
		<form onsubmit="sendMessage(); return false;">
			<input type="text" id="inputbox-content" />
		</form>
	</div>
</div>

<script type="text/javascript" src="/static/chat.js"></script>
</apply>
