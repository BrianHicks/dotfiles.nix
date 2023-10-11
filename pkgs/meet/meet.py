#!/usr/bin/env python3
import argparse
from datetime import datetime, timedelta
import json
import os.path
import re
import subprocess
import sys
from urllib.parse import quote

EMOJI_RE = re.compile(
    "["
    u"\U0001F600-\U0001F64F"  # emoticons
    u"\U0001F300-\U0001F5FF"  # symbols & pictographs
    u"\U0001F680-\U0001F6FF"  # transport & map symbols
    u"\U0001F1E0-\U0001F1FF"  # flags (iOS)
    "]+",
    flags=re.UNICODE
)

PERSON_EMAIL_RE = re.compile(
    r"^\|\s*(?P<link>.+?)\s*\|\s*(?P<email>.+?@.+?)\s*\|$",
)

ATTENDEES_RE = re.compile(r"^attendees: (.+?)$")

DATETIME_RE = re.compile(r"^(.+?) - (.+?)$")


class Event:
    SEPARATOR = '[!!]'

    ICALBUDDY_SEPARATOR = f'|{SEPARATOR}|'
    ICALBUDDY_PROPERTY_ORDER = ['title', 'attendees', 'datetime']
    ICALBUDDY_TIME_FORMAT = '%Y-%m-%dT%H:%M:%S'

    def __init__(self, title, datetime, attendees):
        self.title = title
        self.datetime = datetime
        self.attendees = attendees

    @classmethod
    def from_icalbuddy_line(cls, line):
        sections = line.split(cls.SEPARATOR)

        # title should always be the first section
        title = sections[0]
        event_datetime = None
        attendees = []

        for section in sections[1:]:
            attendees_match = ATTENDEES_RE.match(section)

            if attendees_match:
                attendees.extend(attendees_match.groups()[0].split(', '))
                continue

            datetime_match = DATETIME_RE.match(section)

            if datetime_match:
                start, end = datetime_match.groups()
                event_datetime = (
                    datetime.strptime(start, cls.ICALBUDDY_TIME_FORMAT),
                    datetime.strptime(end, cls.ICALBUDDY_TIME_FORMAT),
                )
                continue

            sys.stderr.write(f"[WARNING] unmatched section: `{section}`\n")

        return cls(title, event_datetime, attendees)

    def __repr__(self):
        return f'<Event: {self.title or "no title"} at {self.datetime or "no time"} with {self.attendees or "nobody"}>'

    def fzf_line(self):
        return f'{self.datetime[0].strftime("%I:%M %p")} to {self.datetime[1].strftime("%I:%M %p")}, {self.title}, {", ".join(self.attendees)}'

    def note_name(self):
        return EMOJI_RE.sub('', self.title).strip().replace(' / ', ', ').replace('/', ', ')

    def duration(self):
        return self.datetime[1] - self.datetime[0]

    def to_json(self):
        return {
            'title': self.title,
            'attendees': self.attendees,
            'datetime': [
                self.datetime[0].isoformat(),
                self.datetime[1].isoformat(),
            ],
        }


def meetings_today(calendars):
    out = subprocess.check_output([
        'icalbuddy',
        '-nc',
        '-b', '',
        '-ic', calendars,
        '-iep', ','.join(Event.ICALBUDDY_PROPERTY_ORDER),
        '-ps', Event.ICALBUDDY_SEPARATOR,
        '-tf', Event.ICALBUDDY_TIME_FORMAT,
        'eventsToday',
    ])

    return [
        Event.from_icalbuddy_line(line)
        for line
        in out.decode('utf-8').strip().split('\n')
    ]


def choose_meeting(calendars):
    meetings = meetings_today(calendars)

    now = datetime.now()
    meetings.sort(key=lambda event: abs(event.datetime[0] - now))

    delimiter = '::::'
    out = subprocess.check_output(
        [
            'fzf',
            f'--delimiter={delimiter}',
            '--with-nth=2',
        ],
        input='\n'.join(
            f'{i}{delimiter}{meeting.fzf_line()}'
            for (i, meeting)
            in enumerate(meetings)
        ).encode('utf-8'),
    ).decode('utf-8').split(delimiter)[0]

    return meetings[int(out)]


def create_note(args, event):
    now = datetime.now().strftime("%Y-%m-%d")
    filename = os.path.join(args.meetings_notes_base, f'{now} {event.note_name()}.md')

    links = prepare_links_to_attendees(args.vault_dir, event)
    with open(os.path.join(args.vault_dir, filename), 'a') as fh:
        fh.write('---\nattendees: ')
        json.dump(links, fh)
        fh.write('\ntitle: ')
        json.dump(event.title, fh)
        fh.write('\nup: ')
        fh.write(event.datetime[0].strftime('"[[%Y-%m-%d]]"'))
        fh.write('\nstart: ')
        fh.write(event.datetime[0].isoformat())
        fh.write('\n---\n')

    subprocess.check_call([
        'open',
        f"obsidian://advanced-uri?vault=Notes&filepath={quote(filename)}", # TODO: parametrize the vault name?
    ])


def prepare_links_to_attendees(vault, event):
    try:
        with open(os.path.join(vault, 'contacts.md'), 'r') as fh:
            lines = fh.readlines()
    except IOError:  # file doesn't exist
        sys.stderr.write("[WARNING] could not read contacts.md!\n")
        return ""

    emails_to_people = {}

    for line in lines:
        match = PERSON_EMAIL_RE.match(line)
        if match is None:
            continue

        groups = match.groupdict()
        emails_to_people[groups['email']] = groups['link']

    people = []

    for attendee in event.attendees:
        try:
            people.append(emails_to_people[attendee])
        except KeyError:
            sys.stderr.write(f"[WARNING] could not find a link for {attendee}\n")
            people.append(attendee)

    return people


def start_montage(event):
    subprocess.check_call([
        'montage',
        'start',
        '--duration', str(round(event.duration() / timedelta(minutes=1))),
        '--meeting',
        event.title,
    ])


if __name__ == '__main__':
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        'command',
        choices=['start', 'note', 'choose'],
        help='What should I do? `start` a meeting or start a `note`?',
    )
    parser.add_argument(
        '--vault-dir',
        default=os.path.expanduser('~/Notes'),
        help='Where is your Obsidian vault?',
    )
    parser.add_argument(
        '--meetings-notes-base',
        default='meetings',
        help='Where should I store notes within your Obsidian vault?',
    )

    args = parser.parse_args()

    meeting = choose_meeting('brian@noredink.com,brian@brianthicks.com')

    if args.command == 'choose':
        json.dump(meeting.to_json(), sys.stdout)
        sys.exit(0)

    if args.command == 'start':
        start_montage(meeting)

    create_note(args, meeting)
