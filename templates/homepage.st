$layout(
    title={Dutch HUG};
    content={
<div class="twocolumn leftcolumn">
<h2>Upcoming</h2>
$Upcoming$

<h2>Previously</h2>
$Previously$
</div>

<div class="twocolumn rightcolumn">
<div id=pics class=pics>
<div><img src=$staticroot$first-meeting-frontpage.jpg width=370 height=278>
<p><span class=white>&#8226;</span><span class=grey>&#8226;</span><span class=grey>&#8226;</span></div>
<div><img src=$staticroot$dutchhugday-2010_03.jpg width=370 height=278>
<p><span class=grey>&#8226;</span><span class=white>&#8226;</span><span class=grey>&#8226;</span></div>
<div><img src=$staticroot$dutchhugday-2010_07.jpg width=370 height=278>
<p><span class=grey>&#8226;</span><span class=grey>&#8226;</span><span class=white>&#8226;</span></div>
</div>
<script>
\$("#pics").cycle("fade");
</script>
$Intro$
</div>
}
)$
