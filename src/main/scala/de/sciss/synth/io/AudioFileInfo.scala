package de.sciss.synth.io

case class AudioFileInfo( fileType: AudioFile.Type, sampleFormat: AudioFile.SampleFormat,
                          numChannels: Int, sampleRate: Double ) 