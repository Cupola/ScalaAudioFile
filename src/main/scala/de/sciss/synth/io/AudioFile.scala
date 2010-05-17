/*
 *  AudioFile.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	 For further information, please contact Hanns Holger Rutz at
 *	 contact@sciss.de
 *
 *
 *  Changelog:
 *		21-May-05	created from de.sciss.eisenkraut.io.AudioFile
 *		15-Jul-05	KEY_APPCODE
 *		28-Aug-05	removed rounding (was buggy) in float<->int conversion
 *					; read/write 16...24 bit int completely noise free now
 *					; optimized buffer handler classes, up to two times faster now
 *		07-Sep-05	marker + region support for WAV, bugfix in WAV readHeader
 *		22-Dec-05	conforms to new contract that allows null-arrays in readFrames
 *		08-Jan-06	added setFrameNum()
 *		21-Feb-06	supports comments (AIFF, IRCAM, SND); don't know how to do it in WAVE (when not linked to a cuepoint)
 *		25-Feb-06	moved to double precision rate
 *		27-May-06	fixed bug in AIFF COMT chunk creation
 *		02-Jul-06	changed AIFF and WAVE read header to allow files > 2 GB
 *		31-Jan-07	added supported for AIFC little endian ; fixed sucky 8-bit WAV
 *		27-Mar-07	separate APPCODE reader, not requiring Application class; separate markers reading;
 *					fixed AIFF output file endian bug
 *		06-Jan-07	added static retrieveType method
 *		10-Sep-08	added Wave64 support
 *    14-May-10   translated to Scala
 */

package de.sciss.synth.io

import java.io.{ BufferedInputStream, DataInputStream, File, FileInputStream, IOException, InputStream,
                 RandomAccessFile }
import java.nio.{ ByteBuffer }
import java.nio.channels.{ Channels }
import math._

/**
 *	 The <code>AudioFile</code> allows reading and writing
 *	 of sound files. It can operate both on a <code>RandomAccessFile</code>
 *  created from a <code>File</code> instance, or on
 *  an kind of <code>InputStream</code> (not every codec will
 *  support this though, and functionality might be limited, for example
 *  seeking is not possible with a plain <code>InputStream</code>).
 *
 *  The codecs are registered with <code>AudioFileType</code>.
 *  The codecs that come with ScalaAudioFile are found in the <code>impl</code>
 *  package.
 *
 *  Reading and writing data requires a user-buffer which holds de-interleaved
 *  floating point data, that is a two dimensional <code>Array</code> which
 *  holds <code>Float</code> data. A type alias <code>Frames</code> is provided
 *  for this, and two helper methods <code>frameBuffer</code>: one static to
 *  construct an arbitrary user-buffer, one in class <code>AudioFile</code>
 *  which creates a buffer with the appropriate channel number.
 *
 *  @author    Hanns Holger Rutz
 *  @version   0.39, 17-May-10
 *
 *  @see    AudioFileType
 *
 *  @todo   openWrite is currently missing
 *          the goodies of ScissLib are missing, e.g. support for
 *          markers, comments, app-code.
 *
 *  @todo   the copyFrames method uses a user-buffer. it should
 *          check for the possibility to directly transfer data
 *          if input and output are compatible.
 */
object AudioFile {
//   private val NAME_LOOP		= "loop"
//   private val NAME_MARK		= "mark"
//   private val NAME_REGION		= "region"

   /**
    *  Opens an audio file for reading.
    *
    *  @param  f  the path name of the file
    *  @return a new <code>AudioFile</code> object
    *          whose header is already parsed and can
    *			   be obtained through the <code>getDescr</code> method.
    *
    *  @throws IOException if the file was not found, could not be read
    *						      or has an unknown or unsupported format
    */
   @throws( classOf[ IOException ])
   def openRead( f: File ) : AudioFile = {
      val raf        = new RandomAccessFile( f, "r" )
      val dis        = dataInput( Channels.newInputStream( raf.getChannel() ))
      val afhr       = createHeaderReader( dis )
      raf.seek( 0L ) // BufferedInputStream did advance the position!
      val afh        = afhr.read( raf )
      val buf        = createBuffer( afh )
      val spec       = afh.spec
      val sf         = spec.sampleFormat
      val br         = sf.readerFactory.map( _.apply( raf.getChannel(), buf, spec.numChannels ))
         .getOrElse( noDecoder( sf ))
      new ReadableFileImpl( f, raf, afh, br )
   }

   @throws( classOf[ IOException ])
   def openRead( is: InputStream ) : AudioFile = {
      val dis        = dataInput( is )
      val afhr       = createHeaderReader( dis )
      val afh        = afhr.read( dis )
      val buf        = createBuffer( afh )
      val spec       = afh.spec
      val sf         = spec.sampleFormat
      val br         = sf.readerFactory.map( _.apply( Channels.newChannel( dis ), buf, spec.numChannels ))
         .getOrElse( noDecoder( sf ))
      new ReadableStreamImpl( dis, afh, br )
   }

   @throws( classOf[ IOException ])
   private def createHeaderReader( dis: DataInputStream ) : AudioFileHeaderReader = {
      val fileType   = identify( dis ).getOrElse( throw new IOException( "Unrecognized audio file format" ))
      val factory    = fileType.factory.getOrElse( noDecoder( fileType ))
      factory.createHeaderReader.getOrElse( noDecoder( fileType ))
   }

   private def createBuffer( afh: AudioFileHeader ) : ByteBuffer = {
      val spec       = afh.spec
      val frameSize  = (spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels
      val bufFrames  = max( 1, 65536 / max( 1, frameSize ))
      val bufSize    = bufFrames * frameSize
		val byteBuf    = ByteBuffer.allocateDirect( bufSize )
		byteBuf.order( afh.byteOrder )
   }

   private def dataInput( is: InputStream ) : DataInputStream =
      new DataInputStream( new BufferedInputStream( is, 1024 ))
   
   private def noDecoder( msg: AnyRef ) = throw new IOException( "No decoder for " + msg )

   /**
    *  Opens an audiofile for reading/writing. The pathname
    *	 is determined bythe <code>file</code> field of the provided <code>AudioFileInfo</code>.
    *	 If a file denotedby this path already exists, it will be
    *	 deleted before opening.
    *	 <p>
    *	 Note that the initial audio file header is written immediately.
    *	 Special tags for the header thus need to be set in the <code>AudioFileInfo</code>
    *	 before calling this method, including markers and regions. It is not
    *	 possible to write markers and regions after the file has been opened
    *	 (since the header size has to be constant).
    *
    *  @param  afd   format and resolution of the new audio file.
    *				      the header is immediatly written to the harddisc
    *
    *  @throws IOException if the file could not be created or the
    *						      format is unsupported
    */
/*
   @throws( classOf[ IOException ])
   def openAsWrite( f: File, spec: AudioFileSpec ) : AudioFile = {
      if( f.exists ) f.delete
      val raf           = new RandomAccessFile( f, "rw" )
      val bh : BufferHandler = null
      val afhw: AudioFileHeaderWriter = null
      val af	         = new WritableImpl( f, raf, bh, afhw )
//      af.afd				= afd
//      afd.len			= 0L
//      af.afh				= af.createHeader
//      af.afh.writeHeader( af.afd )
//      af.init
//      af.seekFrame( 0L )
//      af.updateStep		= afd.rate.toLong * 20
//      af.updateLen		= af.updateStep
//      af.updateTime		= System.currentTimeMillis() + 10000
      af
   }
*/

   def frameBuffer( numChannels: Int, bufFrames: Int = 8192 ) : Frames =
      Array.ofDim[ Float ]( numChannels, bufFrames )

   def readSpec( f: File ) : AudioFileSpec = {
      val raf  = new RandomAccessFile( f, "r" )
      try {
         val dis  = dataInput( Channels.newInputStream( raf.getChannel() ))
         val afhr = createHeaderReader( dis )
         raf.seek( 0L ) // BufferedInputStream did advance the position!
         afhr.read( raf ).spec
      } finally {
         raf.close()
      }
   }

   /**
    *    Note that this method advances in
    *    the provided input stream, its
    *    previous position is not reset.
    */
   @throws( classOf[ IOException ])
   def readSpec( dis: DataInputStream ) : AudioFileSpec = {
      val afhr = createHeaderReader( dis )
      afhr.read( dis ).spec
   }

   /**
    *  Determines the type of audio file.
    *
    *  @param		f   the pathname of the file
    *  @return		the type code as defined in <code>AudioFileInfo</code>,
    *				e.g. <code>TYPE_AIFF</code>. Returns <code>TYPE_UNKNOWN</code>
    *				if the file could not be identified.
    *
    *  @throws IOException if the file could not be read
    */
   @throws( classOf[ IOException ])
   def identify( f: File ) : Option[ AudioFileType ] = {
      val dis = dataInput( new FileInputStream( f ))
      try {
         identify( dis )
      } finally {
         dis.close()
      }
   }

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) : Option[ AudioFileType ] =
      AudioFileType.known.find( _.factory.map( f => {
         dis.mark( 1024 )
         try {
            f.identify( dis )
         } catch {
            case e => false
         } finally {
            dis.reset()
         }
      }).getOrElse( false ))
//            case WAVEHeader.RIFF_MAGIC => {					         // -------- probably WAVE --------
//               if( len >= 12 ) {
//                  raf.readInt()
//                  val magic = raf.readInt()
//                  if( magic == WAVEHeader.WAVE_MAGIC ) Some( Wave )
//                  else None
//               } else None
//            }
//            case Wave64Header.RIFF_MAGIC1a => {				         // -------- probably Wave64 --------
//               if( (len >= 40) &&
//                   (raf.readInt() == Wave64Header.RIFF_MAGIC1b) &&
//                   (raf.readLong() == Wave64Header.RIFF_MAGIC2) ) {
//
//                  raf.readLong() // len
//
//                  if( (raf.readLong() == Wave64Header.WAVE_MAGIC1) &&
//                      (raf.readLong() == Wave64Header.WAVE_MAGIC2) ) Some( Wave64 )
//                  else None
//               } else None
//            }
//            case SNDHeader.SND_MAGIC => Some( NeXT )              // -------- snd --------
//            case IRCAMHeader.IRCAM_VAXBE_MAGIC => Some( IRCAM )	// -------- IRCAM --------
//            case IRCAMHeader.IRCAM_SUNBE_MAGIC => Some( IRCAM )
//            case IRCAMHeader.IRCAM_MIPSBE_MAGIC => Some( IRCAM )

//   @throws( classOf[ IOException ])
//	private def createHeader( fileType: Type ) : Header = {
//		afd.format match {
//         case AIFF   => new AIFFHeader
//         case NeXT   => new SNDHeader
//         case IRCAM  => new IRCAMHeader
//         case Wave   => new WAVEHeader
//         case Raw    => new RawHeader
//         case Wave64 => new Wave64Header
//         case _      => throw new IOException( getResourceString( "errAudioFileType" ))
//		}
//	}

   private trait Basic extends AudioFile {
      protected var framePositionVar: Long = 0L

      protected def afh: AudioFileHeader
      protected def bh: BufferHandler 

      def numFrames : Long       = afh.spec.numFrames
      def framePosition: Long    = framePositionVar
      def spec : AudioFileSpec   = afh.spec

      @throws( classOf[ IOException ])
      def copyFrames( target: AudioFile, len: Long ) : AudioFile = {
         val tempBufSize	= min( len, 8192 ).toInt
         val tempBuf		   = Array.ofDim[ Float ]( spec.numChannels, tempBufSize )
         var remaining     = len

         while( remaining > 0 ) {
            val chunkLen = min( remaining, tempBufSize ).toInt
            readFrames( tempBuf, 0, chunkLen )
            target.writeFrames( tempBuf, 0, chunkLen )
            remaining -= chunkLen
         }
         this
      }

      def cleanUp : AudioFile = {
         try { close } catch { case e: IOException => }
         this
      }

      protected def opNotSupported = throw new IOException( "Operation not supported" )
   }

   private trait Readable extends Basic {
      protected def bh: BufferReader
//      protected def di: DataInput

      @throws( classOf[ IOException ])
      def readFrames( data: Frames, off: Int, len: Int ) : AudioFile = {
         bh.readFrames( data, off, len )
         framePositionVar += len
         this
      }
   }

   private trait ReadOnly extends Readable {
      def isWritable       = false

      @throws( classOf[ IOException ])
      def flush : AudioFile = opNotSupported

      @throws( classOf[ IOException ])
      def writeFrames( data: Frames, off: Int, len : Int ) : AudioFile = opNotSupported

      @throws( classOf[ IOException ])
      def numFrames_=( frames : Long ) : AudioFile = opNotSupported

      @throws( classOf[ IOException ])
      def truncate : AudioFile = opNotSupported

//      @throws( classOf[ IOException ])
//      def close : AudioFile = {
//         dis.close()
//         this
//      }
   }

   private trait StreamLike extends Basic {
      def file: Option[ File ] = None

      @throws( classOf[ IOException ])
      def seekFrame( frame : Long ) : AudioFile = opNotSupported
   }

   private trait FileLike extends Basic {
      protected def f: File
      protected def raf: RandomAccessFile
      
      def file: Option[ File ] = Some( f )

      private val sampleDataOffset = raf.getFilePointer()
      
      @throws( classOf[ IOException ])
      def seekFrame( frame : Long ) : AudioFile = {
         val physical = sampleDataOffset + frame * bh.frameSize
         raf.seek( physical )
         framePositionVar = frame
         this
      }
   }

   private trait ReadOnlyFileLike extends FileLike with ReadOnly {
      @throws( classOf[ IOException ])
      def close : AudioFile = {
         raf.close()
         this
      }
   }

   private trait ReadOnlyStreamLike extends StreamLike with ReadOnly {
      protected def dis: DataInputStream

      @throws( classOf[ IOException ])
      def close : AudioFile = {
         dis.close()
         this
      }
   }

   private class ReadableStreamImpl( protected val dis: DataInputStream, protected val afh: AudioFileHeader,
                                     protected val bh: BufferReader )
   extends ReadOnlyStreamLike {
   }

   private class ReadableFileImpl( protected val f: File,
                                   protected val raf: RandomAccessFile, protected val afh: AudioFileHeader,
                                   protected val bh: BufferReader )
   extends ReadOnlyFileLike {
   }

/*
   private class WritableImpl( _file: Option[ File ], _raf: RandomAccessFile, bh: BufferHandler,
                               afhw: AudioFileHeaderWriter )
   extends Basic {
      private var numFramesVar  = 0L
      private var updateTime : Long = _
      private var updatePos : Long = _
      private val updateStep  = spec.sampleRate.toLong * 20

      // ---- constructor ----
      {
         resetUpdate
      }

      def numFrames: Long = numFramesVar

      private def resetUpdate {
         updateTime	= System.currentTimeMillis() + 10000
         updatePos   = framePosition + updateStep
      }

      @throws( classOf[ IOException ])
      def numFrames_=( frames : Long ) : AudioFile = {
         raf.map( r => {
            val physical = afh.sampleDataOffset + frames * bh.frameSize

            raf.setLength( physical )
            if( framePosition > frames ) framePosition = frames
            numFramesVar = frames
            resetUpdate
            this
         }) getOrElse opNotSupported
      }

      def isWritable = true

      @throws( classOf[ IOException ])
      def flush : AudioFile = {
         afhw.update( numFrames )
         fch.force( true )
         resetUpdate
         this
      }

      @throws( classOf[ IOException ])
      def writeFrames( data: Frames, off: Int, len : Int ) : AudioFile = {
         bh.writeFrames( data, off, len )
         framePosition += len

         if( framePosition > numFrames ) {
            numFrames = framePosition
            if( (framePosition > updatePos) || (System.currentTimeMillis() > updateTime) ) {
               flush
            }
         }
         this
      }

      @throws( classOf[ IOException ])
      def truncate : AudioFile = {
         fch.truncate( fch.position() )
         if( framePosition != numFrames ) {
            numFrames	= framePosition
            afhw.update( numFrames )
            resetUpdate
         }
         this
      }

      @throws( classOf[ IOException ])
      def close : AudioFile = {
         afhw.update( numFrames )
         fch.force( true )
         raf.close()
         this
      }
   }
   */
}

trait AudioFile {
   import AudioFile._

//-------- public methods --------

	/**
	 *  Returns a description of the audio file's specification.
	 */
	def spec: AudioFileSpec

   /**
    *  Returns the underlying <code>File</code> if it was
    *  provided to the <code>AudioFile</code> constructor.
    *  For an <code>AudioFile</code> created from an <code>InputStream</code>
    *  this will return <code>None</code>.
    */
   def file: Option[ File ]

   def isWritable : Boolean

   /**
    *	Reads sample frames from the current position
    *
    *  @param  data	buffer to hold the frames read from harddisc.
    *					the samples will be deinterleaved such that
    *					data[0][] holds the firstchannel, data[1][]
    *					holds the second channel etc.
    *					; it is allowed to have null arrays in the data
    *					(e.g. data[0] == null), in which case these channels
    *					are skipped when reading
    *  @param  off  off in the buffer in sample frames, such
    *					that he firstframe of the first channel will
    *					be placed in data[0][off] etc.
    *  @param  len  number of continuous frames to read.
    *
    *  @throws IOException if a read error or end-of-file occurs.
    */
   @throws( classOf[ IOException ])
   def readFrames( data: Frames, off: Int, len: Int ) : AudioFile

   @throws( classOf[ IOException ])
   final def readFrames( data: Frames ) : AudioFile =
      readFrames( data, 0, data.find( _ != null ).map( _.length ).getOrElse( 0 ))


//   @throws( classOf[ IOException ])
//	private def init {
//		channels		   = afd.channels;
//		bytesPerFrame	= (afd.bitsPerSample >> 3) * channels
//		frameBufCapacity= Math.max( 1, 65536 / Math.max( 1, bytesPerFrame ))
//		byteBufCapacity = frameBufCapacity * bytesPerFrame
//		byteBuf			= ByteBuffer.allocateDirect( byteBufCapacity )
//		byteBuf.order( afh.byteOrder )
//		bh				= null;
//
//		afd.sampleFormat match {
//		case AudioFileInfo.FORMAT_INT => afd.bitsPerSample match {
//			case 8 => 			// 8 bit int
//				if( afh.isUnsignedPCM() ) {
//					bh  = new UByteBufferHandler();
//			   } else {
//					bh  = new ByteBufferHandler();
//            }
//	      case 16 =>		// 16 bit int
//            bh  = new ShortBufferHandler();
//			case 24 =>		// 24 bit int
//				if(afh.getByteOrder() == ByteOrder.BIG_ENDIAN ) {
//					bh  = new ThreeByteBufferHandler();
//				} else {
//					bh  = new ThreeLittleByteBufferHandler();
//			}
//			case 32 =>		// 32 bit int
//				bh  = new IntBufferHandler();
//	   }
//		case AudioFileInfo.FORMAT_FLOAT => afd.bitsPerSample match {
//			case 32 =>		// 32bit float
//				bh  = new FloatBufferHandler();
//			case 64 =>		// 64 bit float
//				bh  = new DoubleBufferHandler();
//			}
//		}
//		if( bh == null ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
//	}

   def frameBuffer( bufFrames: Int = 8192 ) : Frames =
      AudioFile.frameBuffer( numChannels, bufFrames )
   
	/**
	 *  Moves thefile pointer to a specific
	 *  frame.
	 *
	 *  @paramframethe sample frame which should be
	 *					thenew file position. this is really
	 *					the sample index and not thephysical file pointer.
	 *  @throws IOException when a seek error occurs or you try to
	 *						seek past the file's end.
	 */
   @throws( classOf[ IOException ])
	def seekFrame( frame : Long ) : AudioFile
	
	/**
	 *	Flushes pending buffer content, and 
	 *	updates the sound file header information
	 *	(i.e. len fields). Usually you
	 *	will not have to call this method directly,
	 *	unless you pause writing for some time
	 *	and want the file information to appear
	 *	as accurate as possible.
	 */
	def flush : AudioFile
	
	/**
	 *  Returns the current file pointer in sample frames
	 *
	 *  @return		thesample frame index which is the off
	 *				for the next read orwrite operation.
	 *
	 *  @throws IOException		when the position cannot be queried
	 */
	def framePosition : Long

   @throws( classOf[ IOException ])
   final def framePosition_=( frame: Long ) : Unit = seekFrame( frame )

	/**
	 *	Writessample frames to the file starting at thecurrent position.
	 *  If you write past the previous end of the file, the <code>len</code>
	 *  field of the internal <code>AudioFileInfo</code> is updated.
	 *  Since you get a reference from <code>getDescr</code> and not
	 *  a copy, using thisreference to the description will automatically
	 *  give you the correct file len.
	 *
	 *  @param  data	buffer holding the frames to write to harddisc.
	 *					the samples mustbe deinterleaved such that
	 *					data[0][] holds the first channel, data[1][]
	 *					holds the second channel etc.
    *  @param  off  off in the buffer in sample frames, such
	 *					that he first frameof the first channel will
	 *					be read from data[0][off] etc.
	 *  @param  len  number of continuous frames towrite.
	 *
	 *  @throws IOException if a write error occurs.
	 */
   @throws( classOf[ IOException ])
	def writeFrames( data: Frames, off: Int, len : Int ) : AudioFile

   @throws( classOf[ IOException ])
	final def writeFrames( data: Frames ) : AudioFile =
      writeFrames( data, 0, data.find( _ != null ).map( _.length ).getOrElse( 0 ))

	/**
	 *	Returns the number of frames
    *	in the file.
	 *
	 *	@return	the number of sample frames
	 *		in the file. includes pending
    *			buffer content
	 *
	 *	@throws	IOException	this is never thrown
	 *			but declared as of the <code>InterleavedStreamFile</code>
	 *			interface
	 */
	def numFrames: Long

   @throws( classOf[ IOException ])
	def numFrames_=( frames : Long ) : AudioFile

	/**
	 *	Convenience method: Returns the number of channels
	 *	in the file. Same as <code>spec.numChannels</code>.
	 *
	 *	@return	the number of channels
	 */
   final def numChannels : Int = spec.numChannels

   final def sampleRate: Double = spec.sampleRate

   final def sampleFormat: SampleFormat = spec.sampleFormat

   final def fileType: AudioFileType = spec.fileType

	/**
    *	Truncates the file to the size represented
	 *	by the current file position. The file
	 *	must have been opened in write mode.
	 *	Truncation occurs only if frames exist
	 *	beyond the currentfile position, which implicates
	 *	that you have set the position using <code>seekFrame</code>
	 *	to a location before the end of the file.
	 *	The header information is immediately updated.
	 *
	 *	@throws	IOException	if truncation fails
	 */
   @throws( classOf[ IOException ])
	def truncate : AudioFile

	/**
	 *	Copies sample frames from a source sound file
	 *to a target file (either another sound file
	 *or any other class implementing the
	 *	<code>InterleavedStreamFile</code> interface).
	 *	Both files must have the same number of channels.
	 *
	 *	@param	target	to file to copy to from this audio file
	 *	@param	len	the number of frames to copy. Reading
	 *					and writing begins at the current positions
	 *				of both files.
	 *
	 *	@throws	IOException	if a reador write error occurs
	 */
   @throws( classOf[ IOException ])
	def copyFrames( target: AudioFile, numFrames: Long ) : AudioFile

	/**
	 *  Flushes and closes thefile
	 *
	 *  @throws IOException if an error occurs during buffer flush
	 *			or closingthefile.
    */
   @throws( classOf[ IOException ])
	def close : AudioFile

	/**
	 *  Flushes and closes the file. As opposed
	 *	to <code>close()</code>, this does not
	 *	throw any exceptions but simplyignores any errors.
	 *
	 *	@see	#close()
	 */
	def cleanUp : AudioFile

	/**
	 *  Reads markers into the audio file description
	 *  if there areany. This methodsets the <code>KEY_MARKERS</code>
	 *  property of the afd, if markers are available. It sets
	 *  the <code>KEY_LOOP</code> property if a loop span is available.
	 *
	 *	@see	#getDescr()
	 *	@see	AudioFileInfo#KEY_MARKERS
	 *	@seeAudioFileInfo#KEY_LOOP
	 *
	 *	@throws	IOException	if a read or parsing error occurs
	 */
//   @throws( classOf[ IOException ])
//	def readMarkers { afh.readMarkers }

	/**
	 *  Reads application specific code into the audio file description
	 *  if there is such code. This method sets the <code>KEY_APPCODE</code>
	 *  property of the afd. The caller can check the <code>appCode</code>
	 *  field of the afd to ensure that potential app code is relevantto it.
	 *
	 *	@see	#getDescr()
	 *	@see	AudioFileInfo#KEY_APPCODE
	 *	@see	AudioFileInfo#appCode
	 *
	 *	@throws	IOException	if a read or parsing error occurs
	 */
//   @throws( classOf[ IOException ])
//	def readAppCode { afh.readAppCode }
}
