import actr

#from naoqi import ALProxy

actr.load_act_r_model("~/Documents/robot-inner-speech/INNER/inner_model.lisp")

global response, response_time
response = ''
response_time = False


def record_model_speech(model, string):
    """This function is called when the model is supposed to speak. 
    The string is the text to be spoken.
    """

    global response, response_time
    response_time = actr.get_time(True)
    response = string
    print(response)


def record_model_motor(model, key):
    global response, response_time
    #response_time = actr.get_time(True)
    response = key
    print(response)


def demo_table():
    """ Run the ACT-R model.

    It starts with an order or command to the robot (e.g. "put napkin near table") and ends with 
    """

    actr.reset()
    #init()

    # ...when manually setting the sentence (without synthesizer)
    text = "put napkin near table".split()

    onset = 0
    actr.set_parameter_value(":sound-decay-time", 0.4)  #0.3 thread 1
    # actr.set_parameter_value(":save-audicon-history", True)
    actr.add_command("inner-speech-response", record_model_speech,
                     "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")

    actr.install_device(["speech", "microphone"])
    actr.add_command("motor-response", record_model_motor,
                     "Motor model response")
    # actr.monitor_command("pech", "motor-response")    # dead code? the command "pech" does not exist thus it cannot be monitored
    for word in text:
        print(word)
        actr.new_word_sound(word, onset)
        onset = onset + 1.5  # 0.4 thread 1

    actr.run(30)    # run model for up to 30 seconds

    print(actr.chunk_slot_value(actr.buffer_read('goal'), "state"))     # with this line we can get info from a buffer.
    print(actr.chunk_slot_value(actr.buffer_read('aural'), "state"))
    

    '''
    previous line prints "ATTENDING-CONF", which means, the next step would be to send an aural event back to lisp (line 749)
    '''
    
    actr.new_word_sound("yes")
    actr.run(10)

    # actr.remove_command_monitor("output-speech", "inner-speech-response")
    # actr.remove_command("inner-speech-response")


def AL_tts():
    IP = "192.168.0.2"
    tts = ALProxy("ALTextToSpeech", IP, 9559)
    tts.say("Hello my name is Pepper")


#AL_tts()
demo_table()