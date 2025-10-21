from scheduler import Scheduler

class MeetingAgent:
    def __init__(self, scheduler):
        self.scheduler = scheduler

    def process_request(self, text):
        """
        Processes a simple meeting request.
        Example: "Schedule a meeting with John and Jane on 2024-10-26 at 10:00"
        """
        try:
            parts = text.split(" ")
            with_index = parts.index("with")
            on_index = parts.index("on")
            at_index = parts.index("at")

            participants = parts[with_index + 1:on_index]
            participants = [p.replace(",", "") for p in participants if p != "and"]
            date = parts[on_index + 1]
            time = parts[at_index + 1]

            return self.scheduler.schedule_meeting(date, time, participants)
        except ValueError:
            print("Error: Invalid request format.")
            return False
