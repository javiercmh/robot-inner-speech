import actr

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/robin_inner_model.lisp")

global response, response_time
response = ''
response_time = False


def speak(string):
    # make_robin_speak(string)
    print("Robin:", string)


def inner_speech():
    """ Run the ACT-R model.

    It starts with the robot saying "hi"
    """

    actr.reset()

    # ...when manually setting the sentence (without synthesizer)
    text = "put napkin near table".split()

    onset = 0
    actr.set_parameter_value(":sound-decay-time", 0.4)  #0.3 thread 1
    # actr.set_parameter_value(":save-audicon-history", True)

    # set up interface to ACT-R/Lisp
    actr.add_command("speech-response", speak,
                     "Speech model response")
    actr.monitor_command("output-speech", "speech-response")

    actr.install_device(["speech", "microphone"])

    for word in text:
        print(word)
        actr.new_word_sound(word, onset)
        onset = onset + 1.5  # 0.4 thread 1

    actr.run(30)    # run model for up to 30 seconds

    actr.set_chunk_slot_value(actr.buffer_read('goal'), "state", "done")
    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))     # with this line we can get info from a buffer.
    
    '''
    previous line prints "ATTENDING-CONF", which means, the next step would be to send an aural event back to lisp (line 749)
    '''
    
    actr.new_word_sound("yes")
    
    print(actr.chunk_slot_value(actr.buffer_read('aural'), "content"))
    
    actr.run(10)

    # actr.remove_command_monitor("output-speech", "inner-speech-response")
    # actr.remove_command("inner-speech-response")

inner_speech()