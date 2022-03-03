import actr
import re
import speech_recognition as sr

"""
/home/javier/finroc/sources/cpp/libraries/speech_recognition/etc/speech_recog_script.py
"""

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

    log(text)
    INTERACTIONS.append(text)
    

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
        record_interactions(f"Robin: {regular}")
    else:
        record_interactions(f"Robin: {string}")


def text_to_robot(string):
    """Takes human speech and sends it to the model word by word.
    """
    global TIME

    record_interactions(f"Human: {string}")

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
    # for index, name in enumerate(sr.Microphone.list_microphone_names()):
    #     print("Microphone with name \"{1}\" found for `Microphone(device_index={0})`".format(index, name))
    device_index = 3

    recognizer = sr.Recognizer()
    mic = sr.Microphone(device_index=device_index, sample_rate=16000)

    with mic as source:
        recognizer.adjust_for_ambient_noise(source)
        log('[Waiting for human to speak:]')
        audio = recognizer.listen(source)

    # output = recognizer.recognize_sphinx(audio)
    output = recognizer.recognize_google(audio)
    
    log(output)

    return output


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
    
    # print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    # print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))

    while True:
        text = get_human_speech()   # retrieves human speech from mic, changes it to a string to be fed to ACT-R 
        text_to_robot(text)
        actr.run(TIME+10)
        reset_actr()
        if 'goodbye' in text:
            break 

    print('\n'.join(INTERACTIONS))
    

if __name__ == "__main__":
    inner_speech()