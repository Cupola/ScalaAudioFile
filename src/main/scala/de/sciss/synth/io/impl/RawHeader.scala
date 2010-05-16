package de.sciss.synth.io.impl

/*
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