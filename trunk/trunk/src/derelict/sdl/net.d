/*
 * Copyright (c) 2004-2007 Derelict Developers
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the names 'Derelict', 'DerelictSDLNet', nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
module derelict.sdl.net;

private
{
    import derelict.sdl.sdl;
    import derelict.util.loader;
}

//==============================================================================
// TYPES
//==============================================================================
enum : Uint8
{
    SDL_NET_MAJOR_VERSION           = 1,
    SDL_NET_MINOR_VERSION           = 2,
    SDL_NET_PATCHLEVEL              = 5,
}

struct IPaddress
{
    Uint32 host;
    Uint16 port;
}

typedef void* TCPsocket;

enum : uint
{
    INADDR_ANY              = 0x00000000,
    INADDR_NONE             = 0xFFFFFFFF,
    INADDR_BROADCAST        = 0xFFFFFFFF,
    SDLNET_MAX_UDPCHANNELS  = 32,
    SDLNET_MAX_UDPADDRESSES = 4,
}

typedef void* UDPsocket;

struct UDPpacket
{
    int channel;
    Uint8 *data;
    int len;
    int maxlen;
    int status;
    IPaddress address;
}

typedef void* SDLNet_SocketSet;

struct _SDLNet_GenericSocket
{
    int ready;
}
alias _SDLNet_GenericSocket* SDLNet_GenericSocket;

alias SDL_SetError SDLNet_SetError;
alias SDL_GetError SDLNet_GetError;

//==============================================================================
// MACROS
//==============================================================================
void SDL_NET_VERSION(SDL_version* X)
{
    X.major = SDL_NET_MAJOR_VERSION;
    X.minor = SDL_NET_MINOR_VERSION;
    X.patch = SDL_NET_PATCHLEVEL;
}

int SDLNet_TCP_AddSocket(SDLNet_SocketSet set, TCPsocket sock)
{
    return SDLNet_AddSocket(set, cast(SDLNet_GenericSocket)sock);
}

int SDLNet_TCP_DelSocket(SDLNet_SocketSet set, TCPsocket sock)
{
    return SDLNet_DelSocket(set, cast(SDLNet_GenericSocket)sock);
}

int SDLNet_UDP_AddSocket(SDLNet_SocketSet set, UDPsocket sock)
{
    return SDLNet_AddSocket(set, cast(SDLNet_GenericSocket)sock);
}

int SDLNet_UDP_DelSocket(SDLNet_SocketSet set, UDPsocket sock)
{
    return SDLNet_DelSocket(set, cast(SDLNet_GenericSocket)sock);
}

bool SDLNet_SocketReady(TCPsocket sock)
{
    return cast(bool)((sock !is null) && (cast(SDLNet_GenericSocket)sock).ready);
}

bool SDLNet_SocketReady(UDPsocket sock)
{
    return cast(bool)((sock !is null) && (cast(SDLNet_GenericSocket)sock).ready);
}

void SDLNet_Write16(Uint16 value, void* areap)
{
    Uint16* areap16 = cast(Uint16*)areap;
    *areap16 = SDL_SwapBE16(value);
}

Uint16 SDLNet_Read16(void *areap)
{
    Uint16* areap16 = cast(Uint16*)areap;
    return SDL_SwapBE16(*areap16);
}

void SDLNet_Write32(Uint32 value, void* areap)
{
    Uint32* areap32 = cast(Uint32*)areap;
    *areap32 = SDL_SwapBE32(value);
}

Uint32 SDLNet_Read32(void* areap)
{
    Uint32* areap32 = cast(Uint32*)areap;
    return SDL_SwapBE32(*areap32);
}

//==============================================================================
// FUNCTIONS
//==============================================================================
extern(C)
{
typedef SDL_version* function() pfSDLNet_Linked_Version;
pfSDLNet_Linked_Version SDLNet_Linked_Version;

typedef int function() pfSDLNet_Init;
typedef void function() pfSDLNet_Quit;
pfSDLNet_Init           SDLNet_Init;
pfSDLNet_Quit           SDLNet_Quit;

typedef int function(IPaddress*, char*, Uint16) pfSDLNet_ResolveHost;
typedef char* function(IPaddress*) pfSDLNet_ResolveIP;
pfSDLNet_ResolveHost        SDLNet_ResolveHost;
pfSDLNet_ResolveIP          SDLNet_ResolveIP;

typedef TCPsocket function(IPaddress*) pfSDLNet_TCP_Open;
typedef TCPsocket function(TCPsocket) pfSDLNet_TCP_Accept;
typedef IPaddress* function(TCPsocket) pfSDLNet_TCP_GetPeerAddress;
typedef int function(TCPsocket,void*,int) pfSDLNet_TCP_Send;
typedef int function(TCPsocket,void*,int) pfSDLNet_TCP_Recv;
typedef void function(TCPsocket) pfSDLNet_TCP_Close;
pfSDLNet_TCP_Open           SDLNet_TCP_Open;
pfSDLNet_TCP_Accept         SDLNet_TCP_Accept;
pfSDLNet_TCP_GetPeerAddress SDLNet_TCP_GetPeerAddress;
pfSDLNet_TCP_Send           SDLNet_TCP_Send;
pfSDLNet_TCP_Recv           SDLNet_TCP_Recv;
pfSDLNet_TCP_Close          SDLNet_TCP_Close;

typedef UDPpacket* function(int) pfSDLNet_AllocPacket;
typedef int function(UDPpacket*) pfSDLNet_ResizePacket;
typedef void function(UDPpacket*) pfSDLNet_FreePacket;
typedef UDPpacket** function(int,int) pfSDLNet_AllocPacketV;
typedef void function(UDPpacket**) pfSDLNet_FreePacketV;
pfSDLNet_AllocPacket        SDLNet_AllocPacket;
pfSDLNet_ResizePacket       SDLNet_ResizePacket;
pfSDLNet_FreePacket         SDLNet_FreePacket;
pfSDLNet_AllocPacketV       SDLNet_AllocPacketV;
pfSDLNet_FreePacketV        SDLNet_FreePacketV;

typedef UDPsocket function(Uint16) pfSDLNet_UDP_Open;
typedef int function(UDPsocket,int,IPaddress*) pfSDLNet_UDP_Bind;
typedef void function(UDPsocket,int) pfSDLNet_UDP_Unbind;
typedef IPaddress* function(UDPsocket,int) pfSDLNet_UDP_GetPeerAddress;
typedef int function(UDPsocket,UDPpacket**,int) pfSDLNet_UDP_SendV;
typedef int function(UDPsocket,int,UDPpacket*) pfSDLNet_UDP_Send;
typedef int function(UDPsocket,UDPpacket**) pfSDLNet_UDP_RecvV;
typedef int function(UDPsocket,UDPpacket*) pfSDLNet_UDP_Recv;
typedef void function(UDPsocket) pfSDLNet_UDP_Close;
pfSDLNet_UDP_Open           SDLNet_UDP_Open;
pfSDLNet_UDP_Bind           SDLNet_UDP_Bind;
pfSDLNet_UDP_Unbind         SDLNet_UDP_Unbind;
pfSDLNet_UDP_GetPeerAddress SDLNet_UDP_GetPeerAddress;
pfSDLNet_UDP_SendV          SDLNet_UDP_SendV;
pfSDLNet_UDP_Send           SDLNet_UDP_Send;
pfSDLNet_UDP_RecvV          SDLNet_UDP_RecvV;
pfSDLNet_UDP_Recv           SDLNet_UDP_Recv;
pfSDLNet_UDP_Close          SDLNet_UDP_Close;

typedef SDLNet_SocketSet function(int) pfSDLNet_AllocSocketSet;
typedef int function(SDLNet_SocketSet,SDLNet_GenericSocket) pfSDLNet_AddSocket;
typedef int function(SDLNet_SocketSet,SDLNet_GenericSocket) pfSDLNet_DelSocket;
typedef int function(SDLNet_SocketSet,Uint32) pfSDLNet_CheckSockets;
typedef void function(SDLNet_SocketSet) pfSDLNet_FreeSocketSet;
pfSDLNet_AllocSocketSet     SDLNet_AllocSocketSet;
pfSDLNet_AddSocket          SDLNet_AddSocket;
pfSDLNet_DelSocket          SDLNet_DelSocket;
pfSDLNet_CheckSockets       SDLNet_CheckSockets;
pfSDLNet_FreeSocketSet      SDLNet_FreeSocketSet;


} // extern(C)

//==============================================================================
// LOADER
//==============================================================================

private void load(SharedLib lib)
{
    bindFunc(SDLNet_Linked_Version)("SDLNet_Linked_Version", lib);
    bindFunc(SDLNet_Init)("SDLNet_Init", lib);
    bindFunc(SDLNet_Quit)("SDLNet_Quit", lib);
    bindFunc(SDLNet_ResolveHost)("SDLNet_ResolveHost", lib);
    bindFunc(SDLNet_ResolveIP)("SDLNet_ResolveIP", lib);
    bindFunc(SDLNet_TCP_Open)("SDLNet_TCP_Open", lib);
    bindFunc(SDLNet_TCP_Accept)("SDLNet_TCP_Accept", lib);
    bindFunc(SDLNet_TCP_GetPeerAddress)("SDLNet_TCP_GetPeerAddress", lib);
    bindFunc(SDLNet_TCP_Send)("SDLNet_TCP_Send", lib);
    bindFunc(SDLNet_TCP_Recv)("SDLNet_TCP_Recv", lib);
    bindFunc(SDLNet_TCP_Close)("SDLNet_TCP_Close", lib);
    bindFunc(SDLNet_AllocPacket)("SDLNet_AllocPacket", lib);
    bindFunc(SDLNet_ResizePacket)("SDLNet_ResizePacket", lib);
    bindFunc(SDLNet_FreePacket)("SDLNet_FreePacket", lib);
    bindFunc(SDLNet_AllocPacketV)("SDLNet_AllocPacketV", lib);
    bindFunc(SDLNet_FreePacketV)("SDLNet_FreePacketV", lib);
    bindFunc(SDLNet_UDP_Open)("SDLNet_UDP_Open", lib);
    bindFunc(SDLNet_UDP_Bind)("SDLNet_UDP_Bind", lib);
    bindFunc(SDLNet_UDP_Unbind)("SDLNet_UDP_Unbind", lib);
    bindFunc(SDLNet_UDP_GetPeerAddress)("SDLNet_UDP_GetPeerAddress", lib);
    bindFunc(SDLNet_UDP_SendV)("SDLNet_UDP_SendV", lib);
    bindFunc(SDLNet_UDP_Send)("SDLNet_UDP_Send", lib);
    bindFunc(SDLNet_UDP_RecvV)("SDLNet_UDP_RecvV", lib);
    bindFunc(SDLNet_UDP_Recv)("SDLNet_UDP_Recv", lib);
    bindFunc(SDLNet_UDP_Close)("SDLNet_UDP_Close", lib);
    bindFunc(SDLNet_AllocSocketSet)("SDLNet_AllocSocketSet", lib);
    bindFunc(SDLNet_AddSocket)("SDLNet_AddSocket", lib);
    bindFunc(SDLNet_DelSocket)("SDLNet_DelSocket", lib);
    bindFunc(SDLNet_CheckSockets)("SDLNet_CheckSockets", lib);
    bindFunc(SDLNet_FreeSocketSet)("SDLNet_FreeSocketSet", lib);
}



GenericLoader DerelictSDLNet;
static this() {
    DerelictSDLNet.setup(
        "SDL_net.dll",
        "libSDL_net.so, libSDL_net-1.2.so, libSDL_net-1.2.so.0",
        "",
        &load
    );
}
