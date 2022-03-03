import speech_recognition as sr


# list devices
# for index, name in enumerate(sr.Microphone.list_microphone_names()):
#     print("Microphone with name \"{1}\" found for `Microphone(device_index={0})`".format(index, name))

recognizer = sr.Recognizer()
mic = sr.Microphone(device_index=3, sample_rate=16000)

while True:
    with mic as source:
        # recognizer.adjust_for_ambient_noise(source)
        print('Start talking:')
        audio = recognizer.listen(source)

    # output = recognizer.recognize_sphinx(audio)
    output = recognizer.recognize_google(audio)
    print(output)
