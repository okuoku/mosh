/*
 * BufferedFileBinaryInputPort.h - <file binary input port>
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *   Copyright (c) 2009  Kokosabu(MIURA Yasuyuki)  <kokosabu@gmail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: BufferedFileBinaryInputPort.h 1227 2009-02-21 03:01:29Z higepon $
 */

#ifndef __SCHEME_BUFFERED_FILE_BINARY_INPUT_PORT__
#define __SCHEME_BUFFERED_FILE_BINARY_INPUT_PORT__

#include "BinaryInputPort.h"

namespace scheme {

class BufferedFileBinaryInputPort : public BinaryInputPort
{
public:
    BufferedFileBinaryInputPort(int fd);
    BufferedFileBinaryInputPort(ucs4string file);
    BufferedFileBinaryInputPort(const char* file);
    BufferedFileBinaryInputPort(ucs4string file, Object fileOptions, Object bufferMode);
    virtual ~BufferedFileBinaryInputPort();

    ucs4string toString();
    int getU8();
    int lookaheadU8();
    ByteVector* getByteVector(uint32_t size);
    int open();
    int close();
    int fileNo() const;
    virtual bool isClosed() const;
    bool hasPosition() const { return true; }
    bool hasSetPosition() const { return true; }
    Object position() const {
        MOSH_ASSERT(false);
        return Object::Undef;
    }
    bool setPosition(int position)
    {
        MOSH_ASSERT(false);
        return false;
    }

private:
    enum {
        BUF_SIZE = 8192,
    };

    void initializeBuffer();

    int fd_;
    ucs4string fileName_;
    bool isClosed_;
    int u8Buf_;
    enum BufferMode bufferMode_;
    uint8_t* buffer_;
    int bufLen_;
    int bufIdx_;

    int realRead(int fd, uint8_t* buf, size_t count);
    void bufFill();
    int bufRead(uint8_t* data, int reqSize);
    int bufRead1(uint8_t* data);
};

}; // namespace scheme

#endif // __SCHEME_BUFFERED_FILE_BINARY_INPUT_PORT__