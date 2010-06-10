<!doctype html>
<meta charset=utf-8>
<meta name=viewport content="width = 768px">
<link rel=stylesheet href=$staticroot$style.css>
<link rel=apple-touch-icon href=$staticroot$apple-touch-icon.png>
<script src=http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js></script>
<script src=$staticroot$jquery.cycle.min.js></script>
<title>$title$</title>
$extrahead$
<div id=header>
<a href=$approot$><img src=$staticroot$logo.png width=50 height=36 class=logo></a>
<h1><a href=$approot$>Dutch Haskell User Group</a></h1>
</div>
<div id=nav>
<ul>
<li><a href=$approot$AboutUs>About us</a></li>
<li><a href=$approot$Meetings>Meetings</a></li>
<li><a href=$approot$DutchHugDay>Dutch HUG day</a></li>
<li><a href=http://groups.google.com/group/dutch-hug>Mailing list</a></li>
</ul>
</div>

<section class=main>
$content$
</section>
<div id=footer>
<p>
Copyright 2010, Dutch Haskell User Group. View the <a href=http://github.com/tomlokhorst/dutchhug-nl>source code</a> for this website on github.
</div>
