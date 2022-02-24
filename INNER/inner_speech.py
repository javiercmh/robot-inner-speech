import actr

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")


def reset_actr():
    actr.remove_command_monitor("output-speech", "inner-speech-response")
    actr.remove_command("inner-speech-response")

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.4)  #0.3 thread 1

    actr.add_command("inner-speech-response", robin_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])


def robin_speaks(model=None, string=''):
    """Gets what Robin is supposed to say.
    """
    
    # separate inner- from regular speech
    if '(' in string:
        inner, regular = string.split(') ')
        inner = inner.replace('(', '')

        print("Robin (to himself):", inner)
        print("Robin:", regular)
    else:
        print("Robin:", string)


def human_speaks(string):
    """Takes human speech and sends it to the model word by word.
    """
    onset = actr.get_time(True)/1000
    for word in string.split():
        actr.new_word_sound(word, onset)
        onset = onset + 1  # separate words by 1 second (in model-time)
    print("Human:", string)


def inner_speech():
    """ Run the ACT-R model.

    It starts with the robot saying "hi"
    """

    actr.reset()
    actr.set_parameter_value(":sound-decay-time", 0.3)

    actr.add_command("inner-speech-response", robin_speaks,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])

    robin_speaks(string='Hi there! [waving hand]')
    
    # we get our response (but in this case we hardcode it)
    # response = get_human_input()
    text = "oh hello robin"
    human_speaks(text)
    # actr.new_word_sound(text, actr.get_time(True)/1000)

    print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    actr.run(20)    # run model for 20 seconds
    # actr.run_until_time(20)

    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))


    # reset_actr()

    text = "fine thanks"
    human_speaks(text)

    actr.run(20)    # run model for 20 seconds
    

    # actr.set_chunk_slot_value(actr.buffer_read('goal'), "state", "done")
    # print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))
        
    # actr.new_word_sound("yes")
    
    # print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    
    # actr.run(10)
    

if __name__ == "__main__":
    inner_speech()