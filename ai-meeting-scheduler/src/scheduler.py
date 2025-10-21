from datetime import datetime

class Scheduler:
    def __init__(self):
        self.calendar = {}

    def check_availability(self, date, time):
        """Checks if a time slot is available on a given date."""
        if date in self.calendar:
            for scheduled_time, _ in self.calendar[date]:
                if scheduled_time == time:
                    return False
        return True

    def schedule_meeting(self, date, time, participants):
        """Schedules a meeting if the time slot is available."""
        if not self.check_availability(date, time):
            print(f"Error: Time slot {time} on {date} is already booked.")
            return False

        if date not in self.calendar:
            self.calendar[date] = []

        self.calendar[date].append((time, participants))
        print(f"Meeting scheduled for {date} at {time} with {', '.join(participants)}.")
        return True

    def get_schedule_for_date(self, date):
        """Returns the schedule for a specific date."""
        return self.calendar.get(date, [])
