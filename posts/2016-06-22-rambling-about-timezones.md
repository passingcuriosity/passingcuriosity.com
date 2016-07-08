---
title: Rambling about time zones
tags: rambling, notes, time
location: Sydney, New South Wales
excerpt: 
  Here's some rambling about time zones which I decided not to send to
  a mailing list.
---

I wrote this as a reply to a private discussion about ways to handle
date and time values in data processing systems. Rather than continue
a discussion that really wasn't going to lead anywhere interesting I
saved the draft and now I'm posting it here. It's probably not very
interesting or useful but it's been a while since I last posted.

If you are interested in time in programs go read
[falsehoods programmers believe about time][1]. If you are interested
in historical times make sure you're aware of travesties like
[Sydney's pre-standard time offset][2] of 10:04:52.

----

Future times can't, in general, be specified in UTC timestamp and
offset: the definitions of timezones can and do change and can change
with little notice. For dates this doesn't make too much of a
difference (a date, of course, denotes the whole 23, 24, or 25 hour
period and not some particular instant within the day) but values with
higher precision it's possible (though in Australian fairly unlikely)
to convert a future date/time to UTC with a time zone definition which
is *not* the one which actually applies when the time occurs.

There's no particular reason to expect any polity to not change it's
time zone over any particular period (New South Wales did it for the
Olympic and Commonwealth Games; Fiji changed it's DST start/end dates
in 2011, 2012, 2013, and 2014; Israel negotiated the dates in
parliament in every year but now has a regular schedule - except where
it conflicts Jewish new year).

You need to represent a local date and time in the target time zone
and, at some point, commit to using the time zone definitions you
happen to have to interpret the local time into UTC. Ideally this
would be safe for instants in the upcoming 12 months , say, but who
knows? For some polities it might make sense to commit with the time
zone definitions we have now (DST in Australia's eastern states has
settled down in the last few years and there are still a few more
years before they can attempt another referendum to introduce it to
Western Australia) but not in others.

PS: As a person who lives in the world and interacts with people and
organisations around me, I'm using "time" to mean "the numbers which
are supposed to correspond to the the numbers on my clock" and not
"absolute time", "UTC", etc. I generally try to use words like
"instant" and "timestamp" (respectively) to refer to those latter
things.

---

**Update** On Monday, July 4 2016 a meeting of Egypt's cabinet
[cancelled daylight savings time][3] which was due to start on
Thursday, July 7. Three day's notice is enough to upgrade the TZ
database on all your computer systems, right?

[1]: http://infiniteundo.com/post/25326999628/falsehoods-programmers-believe-about-time
[2]: https://github.com/eggert/tz/blob/master/australasia#L187
[3]: http://www.sis.gov.eg/En/Templates/Articles/tmpArticleNews.aspx?ArtID=105572
