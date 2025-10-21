from agent import MeetingAgent
from scheduler import Scheduler

if __name__ == "__main__":
    # Create a scheduler and an agent
    scheduler = Scheduler()
    agent = MeetingAgent(scheduler)

    # Process a sample meeting request
    request = "Schedule a meeting with John and Jane on 2024-10-26 at 10:00"
    agent.process_request(request)

    # Verify the meeting was scheduled
    schedule = scheduler.get_schedule_for_date("2024-10-26")
    print(f"Schedule for 2024-10-26: {schedule}")
