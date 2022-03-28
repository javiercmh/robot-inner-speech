import libfinroc_plugins_interface_api_finroc_interface as finroc
from time import sleep
import actr
import re
import speech_recognition as sr

"""
/home/a_ashok/finroc/sources/cpp/libraries/speech_recognition/etc/speech_recog_script.py
"""

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")

TIME = 0
INTERACTIONS = []

interface = finroc.CreateInterfaceModule("actr_interface", None)
finroc.Initialize("ACTR Inner Speech")
robot_inner_dialog = interface.CreateOutputPort("robot_inner_dialog")
robot_regular_dialog = interface.CreateOutputPort("robot_regular_dialog")

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
    # if publish: strg /
    #robot_dialog.Publish(text,0)
    
def log(text):
    """Log the text to a file.
    """
    print(text)
    with open('output.log', 'a') as f:
        f.write(f"{text}\n")

def robin_speaks(model=None, string=''):
    """Gets what Robin is supposed to say.
    """
    
    # separate inner- from regular speech
    if '(' in string:
        inner, regular = string.split(') ')
        inner = inner.replace('(', '')
        record_interactions(f"Robin (to themself): {inner}")
        robot_inner_dialog.Publish(inner,0)
        record_interactions(f"Robin: {regular}")
        robot_regular_dialog.Publish(regular,0)
    else:
        record_interactions(f"Robin: {string}")
        #robot_dialog.Publish(string)

    #sleep(2)

def text_to_robot(string):
    """Takes human speech and sends it to the model word by word.
    """
    global TIME

    record_interactions(f"Human: {string}")
    #robot_dialog.Publish(string)

    # keep only the word-like characters
    string = re.sub(r'[^\w\s]', '', string)

    for word in string.split():
        actr.new_word_sound(word, TIME)
        TIME = TIME + 1  # separate words by 1 second (in model-time)


def get_human_speech():
    """Text to speech from microphone input.
    Returns string.
    """

    # To determine which mic to use, uncomment next lines and assign the correct device_index
    #for index, name in enumerate(sr.Microphone.list_microphone_names()):
    #    print("Microphone with name \"{1}\" found for `Microphone(device_index={0})`".format(index, name))
    device_index = 7

    recognizer = sr.Recognizer()
    mic = sr.Microphone(device_index=device_index, sample_rate=16000)

    with mic as source:
        recognizer.adjust_for_ambient_noise(source) # listen for 1 second to calibrate the energy threshold for ambient noise levels
        log('[Waiting for human to speak:]')
        audio = recognizer.listen(source) # now when we listen, the energy threshold is already set to a good value, and we can reliably catch speech right away

    # output = recognizer.recognize_sphinx(audio)
    output = recognizer.recognize_google(audio) #used in speech recognition for robothespian

    log(output)

    return output

    #sleep(2)


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

    while True:
        text = get_human_speech()  # retrieves human speech from mic, changes it to a string to be fed to ACT-R
        text_to_robot(text)
        actr.run(TIME + 10)
        reset_actr()
        if 'goodbye' in text:
            break

    print('\n'.join(INTERACTIONS))

    # # we get our response (but in this case we hardcode it)
    # # response = get_human_input()
    # text = "Hello, Robin"
    # human_speaks(text)
    #
    # print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    # actr.run(TIME+10)    # run model for 10 seconds longer than we need
    #
    # print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))
    #
    # reset_actr()    # reset the model to wait for a new input
    # text = "I'm fine"
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "No, I don't like talking with dumb robots."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "I don't like robots in general. I think it would be foolish to integrate them."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "Well, for one, robots are dangerous and could take over the world."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "What happens when robots achieve consciousness and become evil? Their AI could find a way around the limitations you just mentioned."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "Even if AI would be safe, it is a tool that can be harmful when used by the wrong people."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "How would we know if artificial intelligence would be safe and not a threat."
    # human_speaks(text)
    # actr.run(TIME+10)
    #
    # reset_actr()
    # text = "Nope, I am good"
    # human_speaks(text)
    # actr.run(TIME+10)

    # print('\n'.join(INTERACTIONS))

if __name__ == "__main__":
    inner_speech()