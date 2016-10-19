<html>
<head>
<style type="text/css">
body		{font-family: Arial, Helvetica, Verdana, sans-serif;
		 font-size: 14px; font-weight: bold;
		 color: rgb(242,242,242);
		 text-shadow: black 3px 3px 5px;
		 background: rgb(80,80,80) url(http://decuslib.com/images/blue-to-gray.gif) repeat-x;}

div.footer	{position: absolute; bottom: 0px; left: 0px; z-index: -1;
		 height: 100px; width: 100%; 
		 padding: 0px;
		 background-image: url(http://decuslib.com/images/gray-to-puke.gif);
		 background-repeat: repeat-x;
		}

h1		{text-align: center;
		 font-size: 36px;
		 color: rgb(0,0,0);
		 text-shadow: gold 2px 2px 0px, silver 4px 4px 6px;}
h3		{text-align: center}

h1 span		{font-family: Cursive;
		 font-size: 36px;}

hr		{color: rgb(232,232,232)}

p		{text-align: justify;
		 padding-left: 20px; padding-right: 20px}

p.note		{position:absolute; bottom: 50px;
		 font-size: 8px}

p.footer	{position:absolute; bottom: 2px;
		 font-size: 8px}

span		{font-size: 12px; font-weight: normal;}

:link,
:hover,
:active,
:visited	{background: rgb(80,80,80); color: rgb(232,232,232); text-decoration: none;}

a:link,
a:hover,
a:active,
a:visited	{background: rgb(80,80,80); color: rgb(255,255,232); text-decoration: underline;}

.center		{margin-left:auto;
		margin-right:auto;
		text-align:center;}

option		{background:#404040; color:F0F0D0; font-weight:bold;}
option:nth-child(1)	{font-weight:normal; text-decoration:underline;}

</style>

<script language="JavaScript">
window.onresize = function () {
				document.getElementById('footer').style.top = 
				Math.max(document.getElementById('main').clientHeight+50,window.innerHeight-100);
			      }
</script>

</head>
<body> 
<div id="main">
<h1>DECUSLIB<span>rary</span>.COM<span>pendium</span></h1>
<h3>DECUS Library Compendium Authorization</h3>

<p>
The <b>DECUS Library Compendium</b> has been under seige from both <b>Optimum Lightpath</b> Security
and <b>Cyscon GmbH</b> with threats of blacklisting, termination of service, and lawsuits
if the <b>DECUS Library Compendium</b> is not taken down.  These threats are due to the fact
that the <b>DECUS Library Compendium</b> permits direct downloads of executable files which,
as claimed by the aformentions entities, is a security risk.  Nothing on this site is foisted
upon those who visit it and therefore, their charges are without merit, and untrue and libelous.
It is the intent of the Regents of the <b>DECUS Library Compendium</b> to not cower to these
threats and demands.  The information contained in the <b>DECUS Library Compendium</b> is both
valuable and historic information, and volumes of it predate both the <b>Optimum Lightpath</b>
(20-Jun-1994) and <b>Cyscon GmbH</b> (25-Sep-2011) despotism.
</p>
<p>
Therefore, as appeasement to these <a href="http://en.wikipedia.org/wiki/Net_neutrality" target="_blank">internet neutrality</a>
adversaries &mdash; the 
<a href="http://www.urbandictionary.com/define.php?term=numbnuts" target="_blank">numbnuts</a> at <b>Optimum
Lightpath</b> Security and the <a href="http://www.urbandictionary.com/define.php?term=plonkers" target="_blank">plonkers</a>
at <b>Cyscon GmbH</b>, both being collectives of habitual 
<a href="http://www.urbandictionary.com/define.php?term=wankers" target="_blank">wankers</a>
who have been dictating what information 
can be FREELY published, disseminated and shared on the internet &mdash; you must now acknowledge
and accept full responsibility for any and all consequences from the executable files which you 
download from this site.  In fact, you cannot directly download anything considered to be an
executable file which is the reason for you being redirected to this page.
</p>
<p>
Respectfully,
</p>
<p>
The Regents of the <b>DECUS Library Compendium</b> and its librarians.
</p>

<h3>_________________________________________</h3>

<br>
<p>
By correctly answering the question below,
you are acknowledging that you accept <b>FULL RESPONSIBILITY FOR ANY AND ALL CONSEQUENCES FROM</b>
anything downloaded from the <b>DECUS Library Compendium</b>.
</p>
<br>
<p>

	<form method="post" action="authenticate.php" class="center">
	<select name="authenticate" size=1>
<option value="The VMS in OpenVMS refers to:" selected>The VMS in OpenVMS refers to:</option><option value="Vacuum Manifold Sensor">Vacuum Manifold Sensor</option><option value="Valley Middle School">Valley Middle School</option><option value="Vanilla Milk Stout">Vanilla Milk Stout</option><option value="Vanishing Money Supply">Vanishing Money Supply</option><option value="Variable Message Signs">Variable Message Signs</option><option value="Various Mathematical Solutions">Various Mathematical Solutions</option><option value="Vegan Mushroom Soup">Vegan Mushroom Soup</option><option value="Vegetarian Meat Substitute">Vegetarian Meat Substitute</option><option value="Vehicle Motion Sensor">Vehicle Motion Sensor</option><option value="Vending Machine Sandwiches">Vending Machine Sandwiches</option><option value="Venetian Marinara Sauce">Venetian Marinara Sauce</option><option value="Venus Mariner Spacecraft">Venus Mariner Spacecraft</option><option value="Vicks Mentholated Steam">Vicks Mentholated Steam</option><option value="Virginia Moonshine Stillhouse">Virginia Moonshine Stillhouse</option><option value="Virtual Memory System">Virtual Memory System</option><option value="Vital Morbidity Statistics">Vital Morbidity Statistics</option><option value="Voluntary Milking System">Voluntary Milking System</option><option value="Voracious Man-eating Sharks">Voracious Man-eating Sharks</option><option value="Voting Machine Scandals">Voting Machine Scandals</option><option value="Vulcan Mating Sacrifice">Vulcan Mating Sacrifice</option>	</select><input type="submit" value="Authenticate" />
	</form>

</p>
</div>
<div id="footer" class="footer">
<p class="note" style="text-align: right;">Note: This archive was previously maintained by Mark Berryman at SAIC.<br />
                Kudos to Mark and SAIC for maintaining this archive for so many years!</p>

<p class="footer">&copy;2009-2016 Regents of the DECUS Library Compendium<br>Google has indexed 20,500 pages of the DECUS Library Compendium.<br />The DECUS Library Compendium currently indexes 112.0 GB of The DECUS Tapes and other freeware tools.<br></p>
</div>
</body>
<script language="JavaScript">
	document.getElementById('footer').style.top = 
	Math.max(document.getElementById('main').clientHeight+50,window.innerHeight-100);
</script>
</html>
