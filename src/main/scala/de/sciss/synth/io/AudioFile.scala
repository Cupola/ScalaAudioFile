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

import impl.AIFFHeader
import java.io.{ DataInputStream, File, FileInputStream, IOException, InputStream, RandomAccessFile }
import java.nio.{ ByteBuffer, ByteOrder }
import java.nio.channels.{ Channels, FileChannel, NonWritableChannelException, ReadableByteChannel }
import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder, CoderResult }
import math._

/**
 *	The <code>AudioFile</code> allows reading and writing
 *	of sound files. It wraps a <code>RandomAccessFile</code>
 *	and delegates the I/O to subclasses which deal with
 *	the specific sample format and endianess.
 *	<p>
 *	Currently supported formats are: AIFF, IRCAM,
 *  NeXT/Sun (.au), WAVE, and Wave64. Supported resolutions are
 *  8/16/24/32 bit integer and 32/64 bit floating point.
 *  However not all audio formats support all bit depths.
 *  <p>
 *	Not all format combinations are supported, for example
 *	the rather exotic little-endian AIFF, but also
 *	little-endian SND, WAVE 8-bit.
 *	<p>
 *  In order to simplify communication with CSound,
 *  raw output files are supported, raw input files however
 *  are not recognized.
 *  <p>
 *  To create a new <code>AudioFile</code> you call
 *  one of its static methods <code>openRead</code> or
 *  <code>openAsWrite</code>. The format description
 *  is handled by an <code>AudioFileInfo</code> object.
 *	This object also contains information about what special
 *	tags are read/written for which format. For example,
 *	AIFF can read/write markers, and application-specific
 *	chunk, and a gain tag. WAVE can read/write markers and
 *	regions, and a gain tag, etc.
 *	<p>
 *	The <code>AudioFile</code> implements the generic
 *	interface <code>InterleavedStreamFile</code> (which
 *	is likely to be modified in the future) to allow
 *	clients to deal more easily with different sorts
 *	of streaming files, not just audio files.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.39, 15-May-10
 *
 *  @see		AudioFileInfo
 *
 *  @todo		more flexible handling of endianess,
 *				at least SND and IRCAM should support both
 *				versions.
 *
 *	@todo		more tags, like peak information and
 *				channel panning.
 *
 *	@todo		(faster) low-level direct file-to-file
 *				copy in the copyFrames method
 */
object AudioFile {
   type Frames = Array[ Array[ Float ]]

   private val NAME_LOOP		= "loop"
   private val NAME_MARK		= "mark"
   private val NAME_REGION		= "region"

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
      val dis        = new DataInputStream( Channels.newInputStream( raf.getChannel() ))
      val (afh, buf) = openRead( dis )
      val spec       = afh.spec
      val sf         = spec.sampleFormat
      val br         = sf.readerFactory.map( _.apply( raf.getChannel(), buf, spec.numChannels ))
         .getOrElse( noDecoder( sf ))
      new ReadableFileImpl( dis, f, raf, afh, br )
   }

   @throws( classOf[ IOException ])
   def openRead( is: InputStream ) : AudioFile = {
      val dis        = new DataInputStream( is )
      val (afh, buf) = openRead( dis )
      val spec       = afh.spec
      val sf         = spec.sampleFormat
      val br         = sf.readerFactory.map( _.apply( Channels.newChannel( dis ), buf, spec.numChannels ))
         .getOrElse( noDecoder( sf ))
      new ReadableStreamImpl( dis, afh, br )
   }

   @throws( classOf[ IOException ])
   private def openRead( dis: DataInputStream ) : (AudioFileHeader, ByteBuffer) = {
      val fileType   = retrieveType( dis ).getOrElse( throw new IOException( "Unrecognized audio file format" ))
      val factory    = fileType.factory.getOrElse( noDecoder( fileType ))
      val afhr       = factory.createHeaderReader.getOrElse( noDecoder( fileType ))
      val afh        = afhr.read( dis )
      val spec       = afh.spec
      val sf         = spec.sampleFormat
      val buf        = createBuffer( afh )
      (afh, buf)
   }

   private def createBuffer( afh: AudioFileHeader ) : ByteBuffer = {
      val spec       = afh.spec
      val frameSize  = (spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels
      val bufFrames  = max( 1, 65536 / max( 1, frameSize ))
      val bufSize    = bufFrames * frameSize
		val byteBuf    = ByteBuffer.allocateDirect( bufSize )
		byteBuf.order( afh.byteOrder )
   }
   
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
   def retrieveType( f: File ) : Option[ AudioFileType ] = {
      val dis = new DataInputStream( new FileInputStream( f ))
      try {
         retrieveType( dis )
      } finally {
         dis.close()
      }
   }

   @throws( classOf[ IOException ])
   def retrieveType( dis: DataInputStream ) : Option[ AudioFileType ] =
      AudioFileType.known.find( _.factory.map( _.identify( dis )).getOrElse( false ))
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
      var framePosition: Long = 0L

      protected def afh: AudioFileHeader
      protected def bh: BufferHandler 

      def numFrames : Long       = afh.spec.numFrames
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
      protected def dis: DataInputStream

      @throws( classOf[ IOException ])
      def readFrames( data: Frames, off: Int, len: Int ) : AudioFile = {
         bh.readFrames( data, off, len )
         framePosition += len
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

      @throws( classOf[ IOException ])
      def close : AudioFile = {
         dis.close()
         this
      }
   }

   private trait StreamImpl extends Basic {
      def file: Option[ File ] = None

      @throws( classOf[ IOException ])
      def seekFrame( frame : Long ) : AudioFile = opNotSupported
   }

   private trait FileImpl extends Basic {
      protected def f: File
      protected def raf: RandomAccessFile
      
      def file: Option[ File ] = Some( f )

      private val sampleDataOffset = raf.getFilePointer()
      
      @throws( classOf[ IOException ])
      def seekFrame( frame : Long ) : AudioFile = {
         val physical = sampleDataOffset + frame * bh.frameSize
         raf.seek( physical )
         framePosition = frame
         this
      }
   }

   private class ReadableStreamImpl( protected val dis: DataInputStream, protected val afh: AudioFileHeader,
                                     protected val bh: BufferReader )
   extends Basic with ReadOnly with StreamImpl {
   }

   private class ReadableFileImpl( protected val dis: DataInputStream, protected val f: File,
                                   protected val raf: RandomAccessFile, protected val afh: AudioFileHeader,
                                   protected val bh: BufferReader )
   extends Basic with ReadOnly with FileImpl {
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

   def file: Option[ File ]
   def isWritable

	/**
	 *  Returns a description of theaudio file's format.
	 *  Fields which are guaranteed to be filled in, are
	 *  the type (use <code>getType</code>), <code>channels</code>,
	 *  <code>bitsPerSample</code>, <code>sampleFormat</code>,
	 *  <code>rate</code> and <code>len</code>.
	 *
	 *  @return an <code>AudioFileInfo</code> describing
	 *			this audio file.
	 *
	 *  @warning	the returned description is not immutable but
	 *				should be considered read only, do not modify it.
	 *				the fields may change dynamically if the file
	 *				is modified, e.g. the <code>len</code> field
	 *				for a writable file.
	 */
	def spec: AudioFileSpec
	
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
   def numChannels : Int = spec.numChannels

//   def sampleRate: Double = spec.sampleRate

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

// -------- AudioFileHeader implementations --------

/*
	private abstract class AbstractRIFFHeader
	extends AudioFileHeader
	{
		protected static final int ADTL_MAGIC		=0x6164746C;	// 'adtl'
		protected static finalintLABL_MAGIC		= 0x6C61626C;	// 'labl'
		protected static final int LTXT_MAGIC		= 0x6C747874;	// 'ltxt'

		// ltxt purpose for regions
	protected static final int RGN_MAGIC		= 0x72676E20;	// 'rgn '

		// fmt format-code
		protected static final int FORMAT_PCM		= 0x0001;
		protected static final int FORMAT_FLOAT		= 0x0003;
		protected static final int FORMAT_EXT		= 0xFFFE;
		
		
		protected long 		sampleDataOffset;
		protected long		dataLengthOffset;
		protected long		factSmpNumOffset;
		protected long		lastUpdateLength	= 0L;
		protected boolean	isFloat	= false;
		protected boolean	unsignedPCM;
				
		protected AbstractRIFFHeader() {  }
	}
	
	// WAVE is the most stupid and chaotic format. there are dozens
	// of alternatives to say the same thing and i'm too lazy to apply higher
	// order heuristics to find out what the creator application was trying
	// to say.
	//
	// therefore we are simply imitating the behaviour of bias peak
	// in terms of marker storage : use LIST/adtl withlabl chunk for the marker
	// names and ltxt chunks for regions (purposefield == "rgn "). the loop
	// region is stored outside in the smpl chunk.
	//
	// in other words, if an application stores normal regions in the
	// smpl chunk we'll missthem. so what, blame m****s***
	//
	// http://www.borg.com/%7Ejglatt/tech/wave.htm for a discussion
	private class WAVEHeader
	extends AbstractRIFFHeader
	{
		private static final int RIFF_MAGIC		= 0x52494646;	// 'RIFF'
		private static final int WAVE_MAGIC		= 0x57415645;	// 'WAVE' (off 8)

// chunk identifiers
		private static final int FMT_MAGIC		= 0x666D7420;	// 'fmt '
		private static final int FACT_MAGIC		= 0x66616374;	// 'fact'
		private static final int DATA_MAGIC		= 0x64617461;	//'data'
		private static final int CUE_MAGIC		= 0x63756520;	// 'cue '
		private static final int SMPL_MAGIC		= 0x73616D6C;	// 'smpl'
	private static final int INST_MAGIC		= 0x696E7374;	// 'inst'

		// embedded LIST (peak speak) / list (rest of the universe speak) format
		private static final int LIST_MAGIC		= 0x6C697374;	// 'list'
		private static final int LIST_MAGIC2	= 0x4C495354;	// 'LIST'
		
		private long		smplMagicOff		= 0L;
		private long		listMagicOff		= 0L;
		private long		listMagicLen		= 0L;
		private long		cueMagicOff			= 0L;
		private static final long riffLengthOffset = 4L;
		
		protected WAVEHeader() {   }
		
		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i,i1, i2, i3, chunkLen, essentials, magic, dataLen = 0, bpf = 0;
		long	len;

			raf.readInt();		// RIFF
raf.readInt();
len	= raf.len() - 8;
//			len		= (readLittleInt() + 1) & 0xFFFFFFFE;		// Laenge ohne RIFF-Header (Dateilaenge minus 8)
			raf.readInt();		// WAVE
	len	   -= 4;
			chunkLen = 0;
			
for( essentials = 2; (len > 0) && (essentials > 0); ) {
				if( chunkLen != 0 ) raf.seek( raf.getFilePointer()+ chunkLen );	// skip to next chunk
			
				magic		= raf.readInt();
				chunkLen	= (readLittleInt() + 1) & 0xFFFFFFFE;
				len		   -= chunkLen + 8;

		switch( magic ) {
	case FMT_MAGIC:
					essentials--;
					i					= readLittleUShort();		// format
				descr.channels		= readLittleUShort();	// # of channels
					i1					= readLittleInt();			// sample rate (integer)
					descr.rate			= i1;
					i2					= readLittleInt();			// bytes per frame and second (=#chan * #bits/8* rate)
					bpf= readLittleUShort();		// bytes per frame (=#chan * #bits/8)
	descr.bitsPerSample	= readLittleUShort();		// # of bits per sample
					if( ((descr.bitsPerSample& 0x07) != 0) ||
						((descr.bitsPerSample >> 3) * descr.channels != bpf) ||
					((descr.bitsPerSample >>3) * descr.channels * i1 != i2) ) {
			
						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					unsignedPCM			= bpf == 1;

					chunkLen-= 16;

					switch( i ) {
					case FORMAT_PCM:
						descr.sampleFormat = AudioFileInfo.FORMAT_INT;
						break;
					case FORMAT_FLOAT:
						descr.sampleFormat = AudioFileInfo.FORMAT_FLOAT;
						break;
					case FORMAT_EXT:
						if( chunkLen < 24 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i1 = readLittleUShort();	// extension size
						if( i1 < 22 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i2 = readLittleUShort();	// #valid bits per sample
						raf.readInt();				// channel mask, ignore
i3 = readLittleUShort();	// GUID first two bytes
						if( (i2 !=descr.bitsPerSample)||
							((i3 != FORMAT_PCM) &&
							(i3 != FORMAT_FLOAT)) ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
						descr.sampleFormat = i3 == FORMAT_PCM ? AudioFileInfo.FORMAT_INT: AudioFileInfo.FORMAT_FLOAT;
						chunkLen -= 10;
						break;
					default:
						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					break;

				case DATA_MAGIC:
					essentials--;
					sampleDataOffset	= raf.getFilePointer();
					dataLen				= chunkLen;
					break;
				
				case CUE_MAGIC:
					cueMagicOff	= raf.getFilePointer();
					break;

				case LIST_MAGIC:
				case LIST_MAGIC2:
					i	= raf.readInt();
					chunkLen -= 4;
		if( i == ADTL_MAGIC ) {
						listMagicOff = raf.getFilePointer();
						listMagicLen = chunkLen;
		} // if( i == ADTL_MAGIC )
					break;

				case SMPL_MAGIC:
					smplMagicOff = raf.getFilePointer() +28;
					break;
				
		case INST_MAGIC:
					raf.readShort();	// skip UnshiftedNode, FineTune
					i = raf.readByte();	// gain (dB)
					if( i != 0 ) descr.setProperty( AudioFileInfo.KEY_GAIN, new Float( Math.exp(
						(double) i / 20 * Math.log( 10 ))));
				chunkLen-= 3;
					break;
				
	default:
					break;
				} // switch( magic )
			} // for( essentials = 2; (len > 0) && (essentials > 0); )
			if( essentials > 0 ) throw new IOException( getResourceString("errAudioFileIncomplete" ));
			
			descr.len	= dataLen / bpf;
		}
		
protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i, i1, i2, i3;
//			Stringstr;
			Region		region;
			List			markers, regions;
			Marker			marker;
			long			pos, pos2;
	Object			o;

			isFloat = descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires FACT extension
raf.writeInt( RIFF_MAGIC );
			raf.writeInt( 0 );				// Laenge ohne RIFF-Header (Dateilaenge minus 8); unknown now
			raf.writeInt( WAVE_MAGIC );

			// fmt Chunk
			raf.writeInt( FMT_MAGIC );
	writeLittleInt( isFloat ? 18 : 16 );	// FORMAT_FLOAT has extension of size 0
			writeLittleShort( isFloat ? FORMAT_FLOAT : FORMAT_PCM );
	writeLittleShort( descr.channels );
			i1 = (int) (descr.rate + 0.5);
			writeLittleInt( i1 );
			i2 = (descr.bitsPerSample>> 3) * descr.channels;
			writeLittleInt( i1 * i2 );
			writeLittleShort( i2 );
			writeLittleShort( descr.bitsPerSample );
			
			if( isFloat ) raf.writeShort( 0 );

			// fact Chunk
			if( isFloat ) {
				raf.writeInt( FACT_MAGIC );
				writeLittleInt( 4 );
				factSmpNumOffset = raf.getFilePointer();
				raf.writeInt( 0 );
		}
			
			// cue Chunk
			markers  = (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( ((markers != null) && !markers.isEmpty()) || ((regions != null) && !regions.isEmpty())) {
				if( markers == null ) markers = Collections.EMPTY_LIST;
				if( regions == null ) regions = Collections.EMPTY_LIST;
				
				raf.writeInt( CUE_MAGIC );
				i2	= markers.size() + regions.size();
				writeLittleInt( 24 * i2 + 4 );
				writeLittleInt( i2 );
				for( i= 0, i1 = 1; i < markers.size(); i++, i1++ ) {
marker = (Marker) markers.get( i );
					writeLittleInt( i1 );
					writeLittleInt( i1 );
					raf.writeInt( DATA_MAGIC );
					raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
					writeLittleInt( (int) marker.pos );
				}
				for( i = 0; i < regions.size(); i++, i1++ ) {
					region = (Region) regions.get( i );
					writeLittleInt( i1 );
					writeLittleInt( i1 );
					raf.writeInt( DATA_MAGIC );
					raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
					writeLittleInt( (int) region.span.getStart() );
				}
				
				raf.writeInt( LIST_MAGIC );
				pos	= raf.getFilePointer();
				raf.writeInt( 0);
				raf.writeInt( ADTL_MAGIC );
				
				for( i = 0, i1= 1; i < markers.size(); i++, i1++ ) {
		marker	= (Marker) markers.get( i );
					i3		= marker.name.len() + 5;
					raf.writeInt( LABL_MAGIC );
					writeLittleInt( i3 );
					writeLittleInt( i1 );
					raf.writeBytes( marker.name );
				if( (i3 & 1) == 0 ) raf.writeByte( 0 ); else raf.writeShort( 0 );
				}
				
				for( i= 0; i < regions.size(); i++, i1++ ) {
					region	= (Region) regions.get( i );
					i3	= region.name.len() + 5;
					raf.writeInt( LABL_MAGIC );
				writeLittleInt( i3 );
					writeLittleInt( i1 );
				raf.writeBytes( region.name );
					if((i3 & 1) == 0 ) raf.writeByte( 0); else raf.writeShort(0 );
				}
				
				for( i = 0, i1 = markers.size() + 1; i < regions.size(); i++, i1++ ) {
region	= (Region) regions.get( i );
					raf.writeInt( LTXT_MAGIC );
					writeLittleInt( 21 );
					writeLittleInt( i1 );
					writeLittleInt( (int) region.span.getLength() );
					raf.writeInt( RGN_MAGIC);
					raf.writeLong( 0 );		// wCountry, wLanguage, wDialect, wCodePage
raf.writeShort( 0 );	// no name (already specified in 'labl' chunk (zero + pad)
				}
				
				// update 'list' chunk size
				pos2 = raf.getFilePointer();
				i	 = (int)(pos2 - pos - 4);
				if( (i & 1) == 1 ) {
					raf.write( 0 );	// padding byte
					pos2++;
				}
				raf.seek( pos );
				writeLittleInt( i );
				raf.seek( pos2 );
			
	} // if marker or region list not empty

			// smpl Chunk
			region  = (Region) descr.getProperty( AudioFileInfo.KEY_LOOP );
			if( region != null ){
				raf.writeInt( SMPL_MAGIC );
				writeLittleInt( 36 + 24 );
				raf.writeLong( 0 );		// dwManufacturer,dwProduct
				writeLittleInt( (int) (1.0e9 / descr.rate + 0.5) );	// dwSamplePeriod
				writeLittleInt( 69 );	// dwMIDIUnityNote
				raf.writeInt( 0 );		// dwMIDIPitchFraction
				raf.writeLong( 0 );		// dwSMPTEFormat, dwSMPTEOffset
				writeLittleInt( 1 );	// just one loop
				raf.writeInt( 0 );		// no additional chunkinformation
				
				writeLittleInt( 0 );	// loop gets ID 0
				writeLittleInt( 0 );	// normal loop
	writeLittleInt( (int) region.span.getStart() );
				writeLittleInt( (int) region.span.getStop() );
				raf.writeLong( 0 );		// dwFraction, dwPlayCount
			}

		// inst Chunk
			o = descr.getProperty( AudioFileInfo.KEY_GAIN );
			if( o != null ) {
				i1	= Math.max( -64, Math.min( 63, (int) (20 * Math.log( ((Float) o).floatValue() ) / Math.log( 10 ) + 0.5) ));
				raf.writeInt( INST_MAGIC );
				writeLittleInt( 7 );
			raf.writeShort( (69 << 24) | (0 << 16) );	//char: MIDI Note, Detune
				raf.write( i1 );							// char gain (dB)
				raf.writeInt( 0x007F007F );					// char LowNote, HighNote, velLo, char velHi
				raf.write( 0 );								// pad byte
			}

			// data Chunk (Header)
			raf.writeInt( DATA_MAGIC );
			dataLengthOffset = raf.getFilePointer();
raf.writeInt( 0 );
			sampleDataOffset = raf.getFilePointer();
			
			updateHeader( descr );
		}
		
		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
long oldPos	= raf.getFilePointer();
			long len	= raf.len();
			if( len == lastUpdateLength ) return;
			
			if( len >= riffLengthOffset + 4 ) {
				raf.seek( riffLengthOffset );
				writeLittleInt( (int) (len - 8) );								// RIFF Chunk len
			}
			if( isFloat &&(len >= factSmpNumOffset + 4) ) {
				raf.seek( factSmpNumOffset );
				writeLittleInt( (int) (descr.len * descr.channels) );			// fact: Sample-Num XXX check multich.!
			}
			if( len >= dataLengthOffset + 4 ) {
				raf.seek(dataLengthOffset );
				writeLittleInt( (int) (len - (dataLengthOffset + 4)) );			// data Chunk len
			}
			raf.seek( oldPos );
			lastUpdateLength= len;
		}
		
		protected long getSampleDataOffset()
{
			return sampleDataOffset;
		}
		
		protected ByteOrder getByteOrder()
		{
			return ByteOrder.LITTLE_ENDIAN;
		}

		protected boolean isUnsignedPCM()
		{
			return unsignedPCM;
		}
		
		protected void readMarkers()
		throws IOException
		{
			if( (smplMagicOff == 0L) && (listMagicOff == 0L) ) return;
			
			final Map	mapCues= new HashMap();
			final Map	mapCueLengths	= new HashMap();
			final Map	mapCueNames		= new HashMap();
			final long	oldPos			= raf.getFilePointer();
			final List	markers, regions;
		int			i, i1, i2, i3, i4, i5;
			Object		o;
			String		str;
			byte[]		strBuf			= null;

			try {
				if( smplMagicOff> 0L ) {
					raf.seek(smplMagicOff );
					i		  =readLittleInt();	// cSampleLoops
					raf.readInt();					// chunk extension len
		//			chunkLen -= 36;
					if( i > 0 ){
			i1	= readLittleInt(); 	// dwIdentifier
						o	= new Integer( i1 );
						mapCues.remove( o );
						mapCueLengths.remove( o );
						str	= (String) mapCueNames.remove( o );
						if( str == null ) str = NAME_LOOP;
						afd.setProperty( AudioFileInfo.KEY_LOOP, new Region( new Span(
							readLittleInt(), readLittleInt() ), str ));
		//				chunkLen -= 16;
					}
				}
				if( listMagicOff > 0L ) {
		raf.seek( listMagicOff );
					for( long chunkLen = listMagicLen; chunkLen >= 8; ) {
						i	=raf.readInt();		// sub chunkID
						i1	= readLittleInt();
						i2	= (i1 + 1) & 0xFFFFFFFE;	// subchunk len
						chunkLen -= 8;
						switch( i ) {
						case LABL_MAGIC:
							i3		  = readLittleInt();	// dwIdentifier
							i1		 -= 4;
							i2	     -= 4;
							chunkLen -=4;
							if( strBuf == null || strBuf.len < i1 ) {
						strBuf  = new byte[ Math.max( 64, i1 )];
							}
							raf.readFully( strBuf, 0, i1 );	// null-terminated
							mapCueNames.put( new Integer( i3 ), new String( strBuf, 0, i1 - 1 ));
						chunkLen -= i1;
							i2		 -= i1;
							break;
							
						case LTXT_MAGIC:
				i3			= readLittleInt();	// dwIdentifier
							i4			= readLittleInt();	// dwSampleLength (= frames)
							i5			= raf.readInt();	// dwPurpose
							raf.readLong();					// skip wCountry, wLanguage, wDialect, wCodePage
							i1			-= 20;
	i2			-= 20;
							chunkLen	-= 20;
							o = new Integer( i3 );
			if( (i1 > 0) && !mapCueNames.containsKey( o )) {// don't overwritenames
								if( strBuf == null || strBuf.len < i1 ) {
									strBuf  = new byte[ Math.max( 64, i1 )];
								}
								raf.readFully( strBuf, 0, i1 );	// null-terminated
								mapCueNames.put( o, new String( strBuf, 0, i1 - 1 ));
								chunkLen -= i1;
								i2		 -= i1;
		}
							if( (i4 > 0) || (i5 == RGN_MAGIC) ){
								mapCueLengths.put( o, new Integer( i4 ));
							}
							break;
							
						default:
							break;
						}
						if( i2 != 0 ) {
							raf.seek( raf.getFilePointer() + i2 );
chunkLen -= i2;
						}
					} // while( chunkLen >= 8 )
				}
				
				if( cueMagicOff > 0L ) {
raf.seek( cueMagicOff );
					i	= readLittleInt();	// num cues
					for( int j = 0; j < i; j++ ) {
						i1	= readLittleInt();	// dwIdentifier
						raf.readInt();			// dwPosition (ignore, we don't use playlist)
				i2	= raf.readInt();	// should be 'data'
						raf.readLong();			// ignore dwChunkStart and dwBlockStart
						i3	= readLittleInt();	// dwSampleOffset (fails for 64bit space)
	if( i2 == DATA_MAGIC ) {
				mapCues.put( new Integer( i1 ), new Integer( i3 ));
						}
					}
	//	chunkLen -= i * 24 + 4;
				}
	
				// resolve markers and regions
				if(!mapCues.isEmpty() ) {
				markers = new ArrayList();
					regions	= new ArrayList();
					for( Iterator iter = mapCues.keySet().iterator(); iter.hasNext(); ) {
						o	= iter.next();
		i	= ((Integer) mapCues.get( o )).intValue();	// start frame
				str	= (String) mapCueNames.get( o );
						o	= mapCueLengths.get( o );
					if( o == null ) {	// i.e. marker
if( str == null ) str = NAME_MARK;
							markers.add( new Marker( i, str ));
						} else {			// i.e. region
							if( str == null ) str = NAME_REGION;
							regions.add( new Region(new Span( i, ((Integer) o).intValue() ), str ));
						}
					}
					if( !markers.isEmpty() ) afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
					if( !regions.isEmpty() ) afd.setProperty( AudioFileInfo.KEY_REGIONS, regions );
				}
			}
finally {
				raf.seek( oldPos );
			}
		}
	} // class WAVEHeader

	// http://www.vcs.de/fileadmin/user_upload/MBS/PDF/Whitepaper/Informations_about_Sony_Wave64.pdf
	private class Wave64Header
	extends AbstractRIFFHeader
	{
		private static final int RIFF_MAGIC1a	= 0x72696666;	// 'riff'
		private staticfinal int RIFF_MAGIC1b	= 0x2E91CF11;
		private static final long RIFF_MAGIC1	= 0x726966662E91CF11L;
		private static final long RIFF_MAGIC2	= 0xA5D628DB04C10000L;

		private static final long WAVE_MAGIC1	= 0x77617665F3ACD311L;	// 'wave'-XXXX
		private static final long WAVE_MAGIC2	= 0x8CD100C04F8EDB8AL;

		// chunk identifiers
private static final long FMT_MAGIC1	= 0x666D7420F3ACD311L;	// 'fmt '-XXXX
		private static final long FMT_MAGIC2	= 0x8CD100C04F8EDB8AL;
		private static final long FACT_MAGIC1	= 0x66616374F3ACD311L;	// 'fact'-XXXX
		private static final long FACT_MAGIC2	= 0x8CD100C04F8EDB8AL;
		private static final long DATA_MAGIC1	= 0x64617461F3ACD311L;	// 'data'-XXXX
		private static final long DATA_MAGIC2	= 0x8CD100C04F8EDB8AL;

//		private static final long LIST_MAGIC1	= 0x6C6973742F91CF11L; // 'list'-XXXX
//		private static final longLIST_MAGIC2	= 0xA5D628DB04C10000L;
		private static final long MARKER_MAGIC1	= 0x5662F7AB2D39D211L;
		private static final long MARKER_MAGIC2	= 0x86C700C04F8EDB8AL;
		
		private final Charset charset = Charset.forName( "UTF-16LE" );
		private long		markersOffset			= 0L;
		private static final long riffLengthOffset	= 16L;

		protected Wave64Header() {  }
		
		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i, i1, i2, i3, essentials, bpf = 0;
			long	len, magic1, magic2, chunkLen, dataLen = 0;

			raf.readLong(); raf.readLong();		// riff
			len	= readLittleLong();
			raf.readLong(); raf.readLong();// wave
			len	   -= 40;
			chunkLen = 0;
			
//System.out.println( "len = " + len );

			for( essentials = 2; (len >= 24) && (essentials > 0); ){
				if( chunkLen != 0 ) raf.seek( raf.getFilePointer() + chunkLen );	// skip to next chunk
			
				magic1		= raf.readLong();
magic2		= raf.readLong();
				chunkLen	= (readLittleLong() + 7) & 0xFFFFFFFFFFFFFFF8L;
				
//System.out.println( "magic1 = " + magic1 +"; chunkLen = " + chunkLen + "; pos = " + raf.getFilePointer() );
				
				len		   -= chunkLen;
				chunkLen   -= 24;

				if( magic1 == FMT_MAGIC1 && magic2 == FMT_MAGIC2 ) {
					essentials--;
					i					= readLittleUShort();		// format
					descr.channels	= readLittleUShort();		// # of channels
					i1= readLittleInt();			// sample rate (integer)
	descr.rate			= i1;
					i2	= readLittleInt();			// bytes per frame and second (=#chan * #bits/8 * rate)
	bpf		= readLittleUShort();		// bytes per frame(=#chan * #bits/8)
					descr.bitsPerSample	= readLittleUShort();		// # of bits per sample
					if( ((descr.bitsPerSample & 0x07) != 0) ||
						((descr.bitsPerSample >> 3) * descr.channels != bpf) ||
						((descr.bitsPerSample >> 3) * descr.channels * i1 != i2) ) {

						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					unsignedPCM			= bpf == 1;

					chunkLen -=16;

					switch( i ) {
			case FORMAT_PCM:
		descr.sampleFormat = AudioFileInfo.FORMAT_INT;
						break;
					case FORMAT_FLOAT:
						descr.sampleFormat = AudioFileInfo.FORMAT_FLOAT;
						break;
					case FORMAT_EXT:
						if( chunkLen < 24 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i1 = readLittleUShort();	// extension size
						if( i1 < 22 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i2 = readLittleUShort();	// # valid bits per sample
						raf.readInt();				// channel mask, ignore
						i3 = readLittleUShort();	// GUID first two bytes
						if( (i2 != descr.bitsPerSample) ||
							((i3 != FORMAT_PCM) &&
							(i3 != FORMAT_FLOAT)) ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
						descr.sampleFormat =i3 == FORMAT_PCM ? AudioFileInfo.FORMAT_INT : AudioFileInfo.FORMAT_FLOAT;
						chunkLen -= 10;
						break;
					default:
						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					
				} else if( magic1 == DATA_MAGIC1 && magic2 == DATA_MAGIC2 ) {
					essentials--;
					sampleDataOffset	= raf.getFilePointer();
					dataLen				= chunkLen;
	
				} else if( magic1 == MARKER_MAGIC1 && magic2 == MARKER_MAGIC2 ) {
					markersOffset			= raf.getFilePointer();
				}
			} // for( essentials = 2; (len > 0) && (essentials > 0); )
			if( essentials > 0 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
			
			descr.len = dataLen / bpf;
		}
		
		protected void writeHeader( AudioFileInfo descr)
		throws IOException
		{
			final List		markers, regions;
			int				i1, i2;
			String			str;
			Region			region;
			Marker			marker;
			long			pos, pos2, n1, n2;

			isFloat = descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires FACT extension
			raf.writeLong( RIFF_MAGIC1 );
			raf.writeLong( RIFF_MAGIC2 );
			raf.writeLong( 40 );		// Laenge inkl. RIFF-Header (Dateilaenge); unknown now
			raf.writeLong( WAVE_MAGIC1 );
			raf.writeLong( WAVE_MAGIC2 );

			// ---- fmt Chunk----
			raf.writeLong( FMT_MAGIC1 );
			raf.writeLong( FMT_MAGIC2 );
	writeLittleLong( isFloat ? 42 : 40 );  // FORMAT_FLOAT has extension of size 0
			writeLittleShort( isFloat ? FORMAT_FLOAT : FORMAT_PCM );
			writeLittleShort( descr.channels );
			i1 = (int) (descr.rate + 0.5);
		writeLittleInt( i1 );
			i2 = (descr.bitsPerSample >> 3) * descr.channels;
			writeLittleInt( i1 * i2 );
			writeLittleShort( i2 );
			writeLittleShort( descr.bitsPerSample );
			
			if( isFloat ) {
//raf.writeShort( 0 );
	raf.writeLong( 0 ); // actually a short, but six extra bytes to align to 8-byte boundary
			}

			// ---- fact Chunk ----
			if( isFloat ) {
				raf.writeLong( FACT_MAGIC1 );
				raf.writeLong( FACT_MAGIC2 );
				writeLittleLong( 32 );
				factSmpNumOffset = raf.getFilePointer();
//			raf.writeInt( 0 );
				raf.writeLong( 0 ); // i guess it shouldbe long???
			}
			
			// -- marker Chunk ----
			markers  = (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( ((markers != null) && !markers.isEmpty()) || ((regions != null) && !regions.isEmpty()) ) {
				
				final CharsetEncoder	enc		= charset.newEncoder();
				CharBuffer				cbuf	= null;
				ByteBuffer				bbuf	= null;
				final List[] cues = {
					markers == null ? Collections.EMPTY_LIST : markers,
				regions == null? Collections.EMPTY_LIST : regions
				};
				
				raf.writeLong( MARKER_MAGIC1 );
				raf.writeLong( MARKER_MAGIC2 );
		pos	= raf.getFilePointer();
				raf.writeLong( 0 );// updated afterwards
				i2	= cues[ 0 ].size() + cues[ 1 ].size();
			writeLittleInt( i2 );
	// CUE64 structures
				for( int i = 0, id = 1; i < 2; i++ ) {
					for( int j = 0; j < cues[ i ].size(); j++, id++ ) {
						if( i == 0) {
							marker	= (Marker) cues[ i ].get( j );
							n1		= marker.pos;
							n2		= -1;
str		= marker.name;
						} else {
							region = (Region) cues[ i ].get( j);
							n1		= region.span.start;
							n2		= region.span.getLength();
							str		= region.name;
		}
						writeLittleInt( id );	// marker ID
						raf.writeInt( 0 );		// padding
						writeLittleLong( n1 );	// position
						writeLittleLong(n2 );	// len
						
						if( (cbuf == null) || (cbuf.capacity() < str.len()) ) {
							cbuf = CharBuffer.allocate( str.len() + 8 );
							bbuf = ByteBuffer.allocate( (cbuf.capacity() + 1) << 1 );
						}
						cbuf.clear();
						cbuf.put( str );
						cbuf.flip();
						bbuf.clear();
						enc.reset();
	enc.encode( cbuf, bbuf, true );
						enc.flush( bbuf);
						bbuf.putShort( (short) 0 ); // null term
						bbuf.flip();
						
						writeLittleInt( bbuf.remaining() );
		raf.writeInt( 0 );		// padding
//System.out.println( "writing " + bbuf.remaining() + " bytes at " + fch.position() );
						fch.write( bbuf );
					}
				}
				
				// update chunk size
				pos2 = raf.getFilePointer();
				n1	 = pos2 - pos;
//System.out.println( "n1 = " +n1 + "; pos = " + pos + "; pos2 = " + pos2 + "; pad = " + (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L) - n1) );
				final int pad = (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L)- n1);
			for( int i = 0; i < pad; i++ ) raf.write( 0 );	// padding byte
				
				raf.seek( pos );
				writeLittleLong( n1 + 16);
//				writeLittleLong( n1 + 16 + pad );
				raf.seek( pos2 + pad );
	
			} // if marker orregion list not empty

		// data Chunk (Header)
			raf.writeLong( DATA_MAGIC1 );
		raf.writeLong( DATA_MAGIC2 );
			dataLengthOffset = raf.getFilePointer();
			raf.writeLong( 24 );
			sampleDataOffset = raf.getFilePointer();
			
			updateHeader( descr );
		}
	
		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
			final long oldPos	= raf.getFilePointer();
			final long len		= raf.len();
	if( len == lastUpdateLength ) return;
			final long lenM8	= len- 8;
			
			if( lenM8 >= riffLengthOffset ) {
				raf.seek( riffLengthOffset );
// System.out.println( "updateHeader: len = " + len );
				writeLittleLong( len );		// riff Chunk len
			}
			if( isFloat && (lenM8 >= factSmpNumOffset) ) {
				raf.seek( factSmpNumOffset );
				writeLittleLong( descr.len * descr.channels);			// fact: Sample-Num XXX check multich.!
			}
			if( lenM8 >= dataLengthOffset ) {
				raf.seek(dataLengthOffset );
				writeLittleLong( len - (dataLengthOffset - 16) );	// data Chunk len
			}
			raf.seek( oldPos );
			lastUpdateLength =len;
		}
		
		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}
		
		protected ByteOrder getByteOrder()
		{
			return ByteOrder.LITTLE_ENDIAN;
		}

		protected boolean isUnsignedPCM()
		{
			return unsignedPCM;
		}
		
		protected void readMarkers()
throws IOException
		{
//System.out.println( "markersOffset = " + markersOffset );
			if( markersOffset == 0L ) return;
			
			final List				markers	= new ArrayList();
			final List	regions	= new ArrayList();
			final CharsetDecoder	dec		= charset.newDecoder();
			CharBuffer				cbuf	=null;
			ByteBuffer				bbuf	= null;
			long					n1, n2;
			int						numBytes;
			String		str;
			CoderResult				result;
			
			final long oldPos = raf.getFilePointer();
			try {
				raf.seek( markersOffset );
				for( int numCues = readLittleInt(), cue = 0; cue< numCues; cue++ ) {
//System.out.println( "cue " + (cue+1) + " of " + numCues );
					raf.readInt();					// marker ID (ignore)
		raf.readInt(); 					// padding
					n1			= readLittleLong();	// pos
					n2		= readLittleLong();	// len (-1 for markers)
					numBytes	= readLittleInt();	// size of name string in bytes
					raf.readInt(); 					// padding
			
			if( bbuf == null || bbuf.capacity() < numBytes ) {
			bbuf = ByteBuffer.allocate( numBytes + 16 );
						cbuf = CharBuffer.allocate( bbuf.capacity() >> 1 );
			}
					
					bbuf.rewind().limit( numBytes);
					
//System.out.println( "reading " + bbuf.remaining() + " bytes from " + fch.position() );
					
					fch.read( bbuf );
				if( (numBytes >= 2) &&
	    (bbuf.get( numBytes - 2 ) == 0) &&
   (bbuf.get( numBytes -1 ) == 0) ) { // null term
			
						bbuf.rewind().limit( numBytes - 2 );
			} else {
						bbuf.flip();
					}
	cbuf.clear();
					dec.reset();
					result = dec.decode( bbuf, cbuf, true );
					if( result.isError() ) {
						throw new IOException( "Error Reading Cue Name" +
						   (result.isMalformed() ? ": Malformed Input" :
					   (result.isOverflow() ? ": Overflow" :
						   (result.isUnderflow() ? ": Underflow" :
						   (result.isUnmappable() ? ": Unmappable" : "")))));
}
					dec.flush( cbuf );
					cbuf.flip();
		str = cbuf.toString();
					
// System.out.println( "n1 = " + n1 + "; n2 = " + n2 + "; name  = '" + str + "'" );
					
					if( n2 < 0 ) {
						markers.add( new Marker( n1, str ));
					} else {
						regions.add( new Region( new Span( n1, n1 + n2 ), str ));
					}
				}

				afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
				afd.setProperty( AudioFileInfo.KEY_REGIONS, regions );

			}
			finally {
				raf.seek( oldPos );
			}
		}
	} // class Wave64Header
	
	private class SNDHeader
	extends AudioFileHeader
	{
		private static final int SND_MAGIC		= 0x2E736E64;	// '.snd'

		private long sampleDataOffset;
		private long headDataLenOffset= 8L;
		private long lastUpdateLength = 0L;
		
		protected SNDHeader() {  }
		
		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i1,i2;
			String	str;
		
			raf.readInt();  // SND magic
			sampleDataOffset= raf.readInt();
			i2				= raf.readInt();
		i1				= raf.readInt();
			descr.rate		= raf.readInt();
			descr.channels	=raf.readInt();
			str				= readNullTermString();
			
			if( str.len() > 0 ) descr.setProperty( AudioFileInfo.KEY_COMMENT, str );

			switch( i1 ) {
	case 2:	// 8 bit linear
				descr.bitsPerSample	= 8;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 3:	// 16 bit linear
				descr.bitsPerSample	= 16;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 4:	// 24 bit linear
				descr.bitsPerSample	= 24;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 5:	// 32 bit linear
				descr.bitsPerSample	= 32;
				descr.sampleFormat	=AudioFileInfo.FORMAT_INT;
				break;
		case 6:	// 32 bit float
				descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			case 7:	// 64 bit float
				descr.bitsPerSample	= 64;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			default:
				throw new IOException( getResourceString( "errAudioFileEncoding" ));
			}

			descr.len	= i2 / (((descr.bitsPerSample + 7) >> 3) * descr.channels);
		}
		
		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			String str;
		
			str				= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
			sampleDataOffset	= str == null ? 28L : (long) ((28 +str.len()) & ~3);
			raf.writeInt( SND_MAGIC );
			raf.writeInt( (int) sampleDataOffset );
//			raf.writeInt( stream.samples * frameLength );	// len
			raf.writeInt( 0 );

			if( descr.sampleFormat == AudioFileInfo.FORMAT_INT ){
				raf.writeInt( (descr.bitsPerSample >> 3) + 1 );
			} else {
raf.writeInt( (descr.bitsPerSample >> 5) + 5 );
			}
raf.writeInt((int) (descr.rate + 0.5) );
			raf.writeInt( descr.channels );
			
			// comment
			if( str == null ) {
				raf.writeInt( 0 );  // minimum 4 byte character data
			} else {
				raf.writeBytes( str );
				switch( str.len() & 3 ) {
			case 0:
				raf.writeInt( 0 );
					break;
				case 1:
					raf.writeByte( 0 );
					raf.writeShort( 0 );
					break;
				case 2:
					raf.writeShort( 0 );
					break;
				case 3:
			raf.writeByte( 0 );
					break;
				}
			}

//			updateHeader( afd );
		}
		
	protected voidupdateHeader( AudioFileInfo descr )
		throws IOException
		{
			long oldPos;
			long len	= raf.len();
			if( len == lastUpdateLength ) return;
			
		if( len >= headDataLenOffset+ 4 ) {
				oldPos = raf.getFilePointer();
			raf.seek( headDataLenOffset );
				raf.writeInt( (int) (len - sampleDataOffset) );		// data size
				raf.seek( oldPos );
				lastUpdateLength = len;
			}
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
			return ByteOrder.BIG_ENDIAN;
		}
	} // class SNDHeader

	private class IRCAMHeader
	extends AudioFileHeader
	{
		// http://www.tsp.ece.mcgill.ca/MMSP/Documents/AudioFormats/IRCAM/IRCAM.html
		// for details about the different magic cookies
	private static final int IRCAM_VAXBE_MAGIC		= 0x0001A364;
		private static final int IRCAM_SUNBE_MAGIC		= 0x64A30200;
		private static final int IRCAM_MIPSBE_MAGIC		= 0x0003A364;

		private static final short BICSF_END= 0;
//		private static finalshort BICSF_MAXAMP			= 1;
		private static final short BICSF_COMMENT		= 2;
		private static final short BICSF_LINKCODE		= 3;
		private static final short BICSF_VIRTUALCODE= 4;
		private static final short BICSF_CUECODE		= 8;
//		private static final short BICSF_PARENTCODE		= 11;

		private long sampleDataOffset;

		protected IRCAMHeader() { }
		
		protected voidreadHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i1, i2,i3;
		long			l1;
			byte[]			strBuf= null;
			byte[]			strBuf2;
			List			regions		= new ArrayList();
		
			raf.readInt();		// IRCAM magic
			descr.rate		= raf.readFloat();
			descr.channels	=raf.readInt();
			i1				= raf.readInt();

			switch( i1 ) {
			case 1:	// 8 bitlinear
				descr.bitsPerSample	= 8;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 2:	// 16 bit linear
				descr.bitsPerSample	= 16;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 3:	// 24 bit linear; existiert dieser wert offiziell?
				descr.bitsPerSample	= 24;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 0x40004:	// 32 bit linear
				descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 4:	// 32 bit float
		descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			case 8:	// 64 bit float
				descr.bitsPerSample	= 64;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			default:
				throw new IOException( getResourceString( "errAudioFileEncoding" ));
			}

			do {
				i1   = raf.readInt();
				i2	 = i1 & 0xFFFF;		// last short = block size
				i1 >>= 16;	// first short = code
// System.err.println( "next tag: code "+i1+"; len "+i2 );
			switch( i1 ) {
				case BICSF_CUECODE:
					if( strBuf == null ) {
						strBuf = new byte[ 64 ];			// tostore the names
					}
					raf.readFully( strBuf );				// region name
					for( i3 = 0; i3 < 64; i3++ ) {
						if( strBuf[ i3 ] ==0 ) break;
					}
					i1	= raf.readInt();					// begin smp
		i2	= raf.readInt();					// endsmp
					regions.add( new Region( new Span( i1, i2 ), new String( strBuf, 0, i3 )));
					break;
				
				case BICSF_LINKCODE:
			case BICSF_VIRTUALCODE:
					throw new IOException( getResourceString( "errAudioFileEncoding" ));
				
				case BICSF_COMMENT:
					strBuf2	= new byte[ i2 ];
					raf.readFully( strBuf2 );
					descr.setProperty( AudioFileInfo.KEY_COMMENT, new String( strBuf2 ));
					break;
				
				default:
					raf.seek( raf.getFilePointer() + i2 );		// skip unknown code
					break;
				}
			} while( i1 != BICSF_END );
			
			if( !regions.isEmpty() ) {
				descr.setProperty( AudioFileInfo.KEY_REGIONS, regions );
			}
			
			l1			= raf.getFilePointer();
			sampleDataOffset= (l1 + 1023L) & ~1023L;			// aufgerundet auf ganze kilobyte
			l1				= raf.len()- sampleDataOffset;  // dataLen in bytes
			descr.len		= l1/ (((descr.bitsPerSample + 7) >> 3) * descr.channels);
		}
		
		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i1, i2;
			List			regions;
			Region			region;
			byte[]			strBuf;
			long			pos;
			String			str;
		
			raf.writeInt( IRCAM_VAXBE_MAGIC );
	raf.writeFloat( (float) descr.rate );
			raf.writeInt( descr.channels );

			if( (descr.sampleFormat == AudioFileInfo.FORMAT_INT) && (descr.bitsPerSample == 32) ) {
				i1 =0x40004;
			} else {
				i1	= descr.bitsPerSample >> 3;		// 1 = 8bit int, 2 = 16bit lin; 3 = 24 bit, 4 = 32bit float, 8 = 64bit float
			}
			raf.writeInt(i1 );

			// markers + regions, loop
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( regions != null &&!regions.isEmpty() ) {
				i1= (BICSF_CUECODE << 16) + 72;		// short cue-code, short sizeof-cuepoint (64 + 4 + 4)
				strBuf	= new byte[ 64 ];
				strBuf[ 0 ] = 0;
				for( i2 = 0; i2 < regions.size(); i2++ ) {
	region	= (Region) regions.get( i2 );
					raf.writeInt( i1 );		// chunk header
					if( region.name.len() <= 64 ) {
						raf.writeBytes( region.name );
						raf.write( strBuf, 0, 64 - region.name.len() );
					} else {
			raf.writeBytes( region.name.substring( 0, 64 ));
					}
				raf.writeInt( (int) region.span.getStart() );
		raf.writeInt( (int) region.span.getStop());
				}
			}
			
			// comment
			str	= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
			if( str != null ) {
				i1		= (BICSF_COMMENT << 16) | str.len();
				raf.writeInt(i1 );
				raf.writeBytes( str );
			}
			
			raf.writeInt( BICSF_END << 16 );
			pos				= raf.getFilePointer();
			sampleDataOffset= (pos + 1023L) & ~1023L;		// aufgerundet auf ganze kilobyte
			strBuf			= new byte[ (int) (sampleDataOffset - pos) ];
			raf.write( strBuf );							// pad until sample off
		}
		
		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
			// not necessary
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
		return ByteOrder.BIG_ENDIAN;	// XXX at the moment only big endian is supported
}
	} // class IRCAMHeader

	private class RawHeader
	extends AudioFileHeader
	{
		protected RawHeader() { }
		
		// this never get's called because
		//retrieveType will never say it's a raw file
		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{  }
		
	// naturally a raw file doesn't have a header
		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{ }
		
		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{  }
		
		protected long getSampleDataOffset()
		{
			return 0L;
		}
		
		protected ByteOrder getByteOrder()
		{
			return ByteOrder.BIG_ENDIAN;		// XXX check compatibility, e.g. with csound linux
		}
	} // class RawHeader
   */
} // classAudioFile
