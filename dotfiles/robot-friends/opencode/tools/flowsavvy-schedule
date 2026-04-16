#!/usr/bin/osascript -l JavaScript

ObjC.import("EventKit");
ObjC.import("Foundation");

function formatDuration(seconds) {
  if (seconds < 60) {
    return `${seconds} seconds`;
  } else if (seconds < 3600) {
    const mins = Math.floor(seconds / 60);
    return mins === 1 ? "1 minute" : `${mins} minutes`;
  } else if (seconds < 86400) {
    const hrs = Math.floor(seconds / 3600);
    const mins = Math.floor((seconds - hrs * 3600) / 60);
    const hPart = hrs === 1 ? "1 hour" : `${hrs} hours`;
    if (mins === 0) return hPart;
    const mPart = mins === 1 ? "1 minute" : `${mins} minutes`;
    return `${hPart} ${mPart}`;
  } else {
    const days = Math.floor(seconds / 86400);
    return days === 1 ? "1 day" : `${days} days`;
  }
}

function run(argv) {
  if (argv.length < 1) {
    throw new Error("Usage: calendar-events.js <calendar-name> [days-ahead]");
  }

  const calName = argv[0];
  const daysAhead = argv.length > 1 ? parseInt(argv[1], 10) : 30;

  const store = $.EKEventStore.alloc.init;

  if ($.EKEventStore.authorizationStatusForEntityType(0) != 3) {
    store.requestFullAccessToEventsWithCompletion(() => {});
    while ($.EKEventStore.authorizationStatusForEntityType(0) != 3) {
      $.NSRunLoop.currentRunLoop.runUntilDate(
        $.NSDate.dateWithTimeIntervalSinceNow(0.1),
      );
    }
  }

  const now = $.NSDate.date;
  const calendar = $.NSCalendar.currentCalendar;
  const startDate = calendar.startOfDayForDate(now);
  const endDate = $.NSDate.dateWithTimeIntervalSinceReferenceDate(
    startDate.timeIntervalSinceReferenceDate + daysAhead * 86400,
  );

  // Find the target calendar
  const allCalendars = store.calendarsForEntityType(0); // EKEntityTypeEvent
  let targetCal = null;
  for (let i = 0; i < allCalendars.count; i++) {
    const c = allCalendars.objectAtIndex(i);
    if (c.title.js === calName) {
      targetCal = c;
      break;
    }
  }
  if (!targetCal) {
    throw new Error(`No calendar found with name: ${calName}`);
  }

  const predicate = store.predicateForEventsWithStartDateEndDateCalendars(
    startDate,
    endDate,
    $.NSArray.arrayWithObject(targetCal),
  );
  const events = store.eventsMatchingPredicate(predicate);

  const emojiRe =
    /^(\p{Extended_Pictographic}(?:\ufe0f?\u200d\p{Extended_Pictographic}|\ufe0f)*)\s*/u;

  const fmtDate = (d) => d.toLocaleDateString("en-US", {
    weekday: "short", year: "numeric", month: "short", day: "numeric",
  });

  // Monday-based week start key
  function weekOf(d) {
    const day = d.getDay();
    const diff = (day === 0 ? -6 : 1) - day; // offset to Monday
    const mon = new Date(d.getFullYear(), d.getMonth(), d.getDate() + diff);
    return mon.toISOString().slice(0, 10);
  }

  // projects -> weeks -> tasks
  const projectWeeks = {};

  for (let i = 0; i < events.count; i++) {
    const evt = events.objectAtIndex(i);
    const summary = evt.title.js;
    const evtStart = new Date(evt.startDate.timeIntervalSince1970 * 1000);
    const evtEnd = new Date(evt.endDate.timeIntervalSince1970 * 1000);
    const duration = (evtEnd - evtStart) / 1000;

    const match = summary.match(emojiRe);
    const project = match ? match[1] : "";
    const title = match ? summary.slice(match[0].length) : summary;

    const wk = weekOf(evtStart);
    if (!projectWeeks[project]) projectWeeks[project] = {};
    if (!projectWeeks[project][wk]) projectWeeks[project][wk] = {};

    const task = projectWeeks[project][wk][title] ?? { occurrences: 0, duration: 0 };
    task.occurrences += 1;
    task.duration += duration;
    projectWeeks[project][wk][title] = task;
  }

  const jsStart = new Date(startDate.timeIntervalSince1970 * 1000);
  const jsEnd = new Date(endDate.timeIntervalSince1970 * 1000);

  let totalEvents = 0;
  for (const proj of Object.values(projectWeeks))
    for (const wk of Object.values(proj))
      for (const t of Object.values(wk))
        totalEvents += t.occurrences;

  const lines = [];
  lines.push(`# ${calName}: ${fmtDate(jsStart)} to ${fmtDate(jsEnd)}`);
  lines.push("");
  lines.push(`${totalEvents} events.`);
  lines.push("");

  for (const [project, weeks] of Object.entries(projectWeeks)) {
    lines.push(`## ${project || "(no project)"}`);
    lines.push("");

    for (const wk of Object.keys(weeks).sort()) {
      const weekDate = new Date(wk + "T00:00:00");
      lines.push(`### Week of ${fmtDate(weekDate)}`);
      lines.push("");
      for (const [title, { occurrences, duration }] of Object.entries(weeks[wk])) {
        const time = occurrences > 1
          ? `${occurrences}x, ${formatDuration(duration)}`
          : formatDuration(duration);
        lines.push(`- ${title} (${time})`);
      }
      lines.push("");
    }
  }

  return lines.join("\n");
}
