/*
 *  AudioFileType.scala
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
 */

package de.sciss.synth.io

import impl._
import java.io.DataInputStream

abstract /* sealed */ class AudioFileType( val id: String, val ext: String ) {
   def factory: Option[ AudioFileHeaderFactory ]
}

object AudioFileType {
   private val sync  = new AnyRef
   private var set   = Set[ AudioFileType ]( AIFF, NeXT, Wave, IRCAM, Wave64 )

   def register( fileType: AudioFileType ) {
      sync.synchronized { set += fileType }
   }

   /**
    *    Note: this Iterator does not include the Raw type
    *    which usually requires special handling.
    */
   def known : Set[ AudioFileType ] = sync.synchronized { set }

   case object AIFF   extends AudioFileType( "aiff",  "aif" ) {
      def factory = Some( AIFFHeader )
   }
   case object NeXT   extends AudioFileType( "next",  "snd" ) {
      def factory = None // XXX
   }
   case object Wave   extends AudioFileType( "wav",   "wav" ) {
      def factory = None // XXX
   }
   case object IRCAM  extends AudioFileType( "ircam", "irc ") {
      def factory = None // XXX
   }
   case object Raw    extends AudioFileType( "raw",   "raw" ) {
      def factory = None // XXX
   }
   case object Wave64 extends AudioFileType( "w64",   "w64" ) {
      def factory = None // XXX
   }
}