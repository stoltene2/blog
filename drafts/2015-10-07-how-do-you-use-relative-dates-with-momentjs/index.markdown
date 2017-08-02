---
title: How do you use relative dates with momentjs?
author: Eric Stolten
published: false
---

1. Change the date of this file
2. Make title catchy, 10 things to be aware of with momentjs
3. Make a plunkr with the examples
4. Be careful of daylight savings
5. Be careful of mutability
6. Always be cloning

~~~ {.javascript .html}
var euroTimeStr = '2015-07-07T10:29:00+01:00'
var euroTimeZone = moment('2015-07-07T10:29:00+01:00', moment.ISO_8601).zone();

console.log('euroTimeZone', euroTimeZone);

// Now in +1 time
var nowInThereTime = moment().zone(euroTimeZone);

console.log('there hours: ', nowInThereTime.hour()); // 16
console.log('there mins: ', nowInThereTime.minute()); // 30
console.log('nowInThereTime', nowInThereTime.format())

var thereInThereTime = moment(euroTimeStr, moment.ISO_8601).zone(euroTimeStr);

console.log('there local hours: ', thereInThereTime.hour()); // 10
console.log('there local mins: ', thereInThereTime.minute()); // 29
console.log('thereInThereTime', thereInThereTime.format())
~~~



~~~ {.javascript .html}
var euroTimeStr = '2015-07-07T10:29:00+02:00'
var euroTimeZone = moment(euroTimeStr, moment.ISO_8601).utcOffset();
var euroTimeZoneUsingZone = moment(euroTimeStr, moment.ISO_8601).zone();

// Now time
var now = moment();

// Now in +2hr TZ time
var nowInThereTime = moment().utcOffset(-euroTimeZone);

// Explicitly uses european timezone
var thereInEuroTime = moment(euroTimeStr, moment.ISO_8601).utcOffset(euroTimeStr);

// This implicitly uses local timezone
var thereInLocalTime = moment(euroTimeStr, moment.ISO_8601);

console.log('euroTimeZone', euroTimeZone);
// This will be nagative of euroTimeZone
console.log('euroTimeZoneUsingZone', euroTimeZoneUsingZone);
console.log('');

console.log('using now');
console.log('now here hours: ', now.hour());
console.log('now here mins: ', now.minute());
console.log('now', now.format('LT'))
console.log('');

console.log('now there hours: ', nowInThereTime.hour());
console.log('now there mins: ', nowInThereTime.minute());
console.log('nowInThereTime', nowInThereTime.format('LT'))
console.log('');

console.log('using the fixed time above with the time there for them locally');
console.log('there euro hours: ', thereInEuroTime.hour());
console.log('there euro mins: ', thereInEuroTime.minute());
console.log('thereInEuroTime', thereInEuroTime.format('LT'))
console.log('');

console.log('using the fixed time above with the time there displayed in our local time');
console.log('there here hours: ', thereInLocalTime.hour());
console.log('there here mins: ', thereInLocalTime.minute());
console.log('thereInLocalTime', thereInLocalTime.format('LT'))
~~~

~~~ {.javascript .html}
euroTimeZone -240
euroTimeZoneUsingZone 240

using now
now here hours:  11
now here mins:  9
now 11:09 AM

now there hours:  19
now there mins:  9
nowInThereTime 7:09 PM

using the fixed time above with the time there for them locally
there euro hours:  10
there euro mins:  29
thereInEuroTime 10:29 AM

using the fixed time above with the time there displayed in our local time
there here hours:  4
there here mins:  29
thereInLocalTime 4:29 AM
~~~
