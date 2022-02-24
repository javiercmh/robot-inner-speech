import actr
import re

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")

TIME = 0
INTERACTIONS = []


def reset_actr():
    TIME = actr.get_time(True)/1000

    actr.remove_command_monitor("output-speech", "inner-speech-response")
    actr.remove_command("inner-speech-response")

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.3)

    actr.add_command("inner-speech-response", robin_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])


def record_interactions(text):
    """Record the interactions between the human and the robot.
    """
    global INTERACTIONS

    INTERACTIONS.append(text)
    print(text)
    

def robin_speaks(model=None, string=''):
    """Gets what Robin is supposed to say.
    """
    
    # separate inner- from regular speech
    if '(' in string:
        inner, regular = string.split(') ')
        inner = inner.replace('(', '')

        record_interactions(f"Robin (to themself): {inner}")
        record_interactions(f"Robin: {regular}")
    else:
        record_interactions(f"Robin: {string}")


def human_speaks(string):
    """Takes human speech and sends it to the model word by word.
    """
    global TIME

    record_interactions(f"Human: {string}")

    # keep only the word-like characters
    string = re.sub(r'[^\w\s]', '', string)

    for word in string.split():
        actr.new_word_sound(word, TIME)
        TIME = TIME + 1  # separate words by 1 second (in model-time)


def inner_speech():
    """ Run the ACT-R model.

    It starts with the robot saying "hi"
    """
    global TIME
    global INTERACTIONS

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.3)

    actr.add_command("inner-speech-response", robin_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])

    robin_speaks(string='Hi there! [waving hand]')
    
    # we get our response (but in this case we hardcode it)
    # response = get_human_input()
    text = "Hello, Robin"
    human_speaks(text)

    print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    actr.run(TIME+10)    # run model for 10 seconds longer than we need

    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))

    reset_actr()    # reset the model to wait for a new input
    text = "I'm fine"
    human_speaks(text)
    actr.run(TIME+10)
    
    reset_actr()
    text = "No, I don't like talking with dumb robots."
    human_speaks(text)
    actr.run(TIME+10)

    reset_actr()
    text = "I don't like robots in general. I think it would be foolish to integrate them."
    human_speaks(text)
    actr.run(TIME+10)

    reset_actr()
    text = "Well, for one, robots are dangerous and could take over the world."
    human_speaks(text)
    actr.run(TIME+10)
    
    reset_actr()
    text = "What happens when robots achieve consciousness and become evil? Their AI could find a way around the limitations you just mentioned."
    human_speaks(text)
    actr.run(TIME+10)
    
    reset_actr()
    text = "Even if AI would be safe, it is a tool that can be harmful when used by the wrong people."
    human_speaks(text)
    actr.run(TIME+10)
    
    reset_actr()
    text = "How would we know if artificial intelligence would be safe and not a threat."
    human_speaks(text)
    actr.run(TIME+10)
    
    reset_actr()
    text = "Nope, I am good"
    human_speaks(text)
    actr.run(TIME+10)

    print('\n'.join(INTERACTIONS))
    

if __name__ == "__main__":
    inner_speech()