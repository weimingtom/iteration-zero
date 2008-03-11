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
 * * Neither the names 'Derelict', 'DerelictSDLMixer', nor the names of its contributors
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
module derelict.sdl.mixer;

private
{
    import derelict.sdl.types;
    import derelict.sdl.rwops;
    import derelict.sdl.audio;
    import derelict.sdl.byteorder;
    import derelict.sdl.sdlversion;
    import derelict.sdl.error;
    import derelict.util.loader;
    import derelict.util.wrapper;
}

//==============================================================================
// Types
//==============================================================================
enum : Uint8
{
    SDL_MIXER_MAJOR_VERSION     = 1,
    SDL_MIXER_MINOR_VERSION     = 2,
    SDL_MIXER_PATCHLEVEL        = 7,
}
alias SDL_MIXER_MAJOR_VERSION MIX_MAJOR_VERSION;
alias SDL_MIXER_MINOR_VERSION MIX_MINOR_VERSION;
alias SDL_MIXER_PATCHLEVEL MIX_PATCH_LEVEL;

alias SDL_SetError   Mix_SetError;
alias SDL_GetError   Mix_GetError;

struct Mix_Chunk {
   int allocated;
   Uint8 *abuf;
   Uint32 alen;
   Uint8 volume;
};

enum Mix_Fading {
   MIX_NO_FADING,
   MIX_FADING_OUT,
   MIX_FADING_IN
};

enum Mix_MusicType {
   MUS_NONE,
   MUS_CMD,
   MUS_WAV,
   MUS_MOD,
   MUS_MID,
   MUS_OGG,
   MUS_MP3
};

struct _Mix_Music {}
typedef _Mix_Music Mix_Music;

enum
{
    MIX_CHANNELS              = 8,
    MIX_DEFAULT_FREQUENCY     = 22050,
    MIX_DEFAULT_CHANNELS      = 2,
    MIX_MAX_VOLUME            = 128,
    MIX_CHANNEL_POST          = -2,
}

version (LittleEndian) {
    enum { MIX_DEFAULT_FORMAT = AUDIO_S16LSB }
} else {
    enum { MIX_DEFAULT_FORMAT = AUDIO_S16MSB }
}

const char[] MIX_EFFECTSMAXSPEED    = "MIX_EFFECTSMAXSPEED";

extern(C)
{
typedef void function(int chan, void* stream, int len, void* udata) Mix_EffectFunc_t;
typedef void function(int chan, void* udata) Mix_EffectDone_t;
}

//==============================================================================
// Macros
//==============================================================================
void SDL_MIXER_VERSION(SDL_version* X)
{
    X.major = SDL_MIXER_MAJOR_VERSION;
    X.minor = SDL_MIXER_MINOR_VERSION;
    X.patch = SDL_MIXER_PATCHLEVEL;
}
alias SDL_MIXER_VERSION MIX_VERSION;


Mix_Chunk* Mix_LoadWAV(char[] file)
{
    return Mix_LoadWAV_RW(SDL_RWFromFile(toCString(file), toCString("rb")), 1);
}

int Mix_PlayChannel(int channel, Mix_Chunk* chunk, int loops)
{
    return Mix_PlayChannelTimed(channel, chunk, loops, -1);
}

int Mix_FadeInChannel(int channel, Mix_Chunk* chunk, int loops, int ms)
{
    return Mix_FadeInChannelTimed(channel, chunk, loops, ms, -1);
}


//==============================================================================
// Functions
//==============================================================================
extern (C)
{
typedef SDL_version* function() pfMix_Linked_Version;
typedef int function (int frequency, Uint16 format, int channels, int chunksize) pfMix_OpenAudio;
typedef int function(int numchans) pfMix_AllocateChannels;
typedef int function(int* frequency, Uint16* format, int* channels) pfMix_QuerySpec;
typedef Mix_Chunk* function(SDL_RWops* src, int freesrc) pfMix_LoadWAV_RW;
typedef Mix_Music* function(char* file) pfMix_LoadMUS;
typedef Mix_Music* function(SDL_RWops* rw) pfMix_LoadMUS_RW;
typedef Mix_Chunk* function(Uint8* mem) pfMix_QuickLoad_WAV;
typedef Mix_Chunk* function(Uint8* mem, Uint32 len) pfMix_QuickLoad_RAW;
typedef void function(Mix_Chunk* chunk) pfMix_FreeChunk;
typedef void function(Mix_Music* music) pfMix_FreeMusic;
typedef Mix_MusicType function(Mix_Music* music) pfMix_GetMusicType;
typedef void function(void (*mix_func)(void* udata, Uint8* stream, int len), void* arg) pfMix_SetPostMix;
typedef void function(void (*mix_func)(void* udata, Uint8* stream, int len), void* arg) pfMix_HookMusic;
typedef void function(void (*music_finished)()) pfMix_HookMusicFinished;
typedef void*  function() pfMix_GetMusicHookData;
typedef void function(void (*channel_finished)(int channel)) pfMix_ChannelFinished;
typedef int function(int chan, Mix_EffectFunc_t f, Mix_EffectDone_t d, void* arg) pfMix_RegisterEffect;
typedef int function(int channel, Mix_EffectFunc_t f) pfMix_UnregisterEffect;
typedef int function(int channel) pfMix_UnregisterAllEffects;
typedef int function(int channel, Uint8 left, Uint8 right) pfMix_SetPanning;
typedef int function(int channel, Sint16 angle, Uint8 distance) pfMix_SetPosition;
typedef int function(int channel, Uint8 distance) pfMix_SetDistance;
typedef int function(int channel, int flip) pfMix_SetReverseStereo;
typedef int function(int num) pfMix_ReserveChannels;
typedef int function(int which, int tag) pfMix_GroupChannel;
typedef int function(int from, int to, int tag) pfMix_GroupChannels;
typedef int function(int tag) pfMix_GroupAvailable;
typedef int function(int tag) pfMix_GroupCount;
typedef int function(int tag) pfMix_GroupOldest;
typedef int function(int tag) pfMix_GroupNewer;
typedef int function(int channel, Mix_Chunk* chunk, int loops, int ticks) pfMix_PlayChannelTimed;
typedef int function(Mix_Music* music, int loops) pfMix_PlayMusic;
typedef int function(Mix_Music* music, int loops, int ms) pfMix_FadeInMusic;
typedef int function(Mix_Music* music, int loops, int ms, double position) pfMix_FadeInMusicPos;
typedef int function(int channel, Mix_Chunk* chunk, int loops, int ms, int ticks) pfMix_FadeInChannelTimed;
typedef int function(int channel, int volume) pfMix_Volume;
typedef int function(Mix_Chunk* chunk, int volume) pfMix_VolumeChunk;
typedef int function(int volume) pfMix_VolumeMusic;
typedef int function(int channel) pfMix_HaltChannel;
typedef int function(int tag) pfMix_HaltGroup;
typedef int function() pfMix_HaltMusic;
typedef int function(int channel, int ticks) pfMix_ExpireChannel;
typedef int function(int which, int ms) pfMix_FadeOutChannel;
typedef int function(int tag, int ms) pfMix_FadeOutGroup;
typedef int function(int ms) pfMix_FadeOutMusic;
typedef Mix_Fading function() pfMix_FadingMusic;
typedef Mix_Fading function(int which) pfMix_FadingChannel;
typedef void function(int channel) pfMix_Pause;
typedef void function(int channel) pfMix_Resume;
typedef int function(int channel) pfMix_Paused;
typedef void function() pfMix_PauseMusic;
typedef void function() pfMix_ResumeMusic;
typedef void function() pfMix_RewindMusic;
typedef int function() pfMix_PausedMusic;
typedef int function(double position) pfMix_SetMusicPosition;
typedef int function(int channel) pfMix_Playing;
typedef int function() pfMix_PlayingMusic;
typedef int function(char* command) pfMix_SetMusicCMD;
typedef int function(int value) pfMix_SetSynchroValue;
typedef int function() pfMix_GetSynchroValue;
typedef Mix_Chunk*  function(int channel) pfMix_GetChunk;
typedef void function() pfMix_CloseAudio;

pfMix_Linked_Version            Mix_Linked_Version;
pfMix_OpenAudio                 Mix_OpenAudio;
pfMix_AllocateChannels          Mix_AllocateChannels;
pfMix_QuerySpec                 Mix_QuerySpec;
pfMix_LoadWAV_RW                Mix_LoadWAV_RW;
pfMix_LoadMUS                   Mix_LoadMUS;
pfMix_LoadMUS_RW                Mix_LoadMUS_RW;
pfMix_QuickLoad_WAV             Mix_QuickLoad_WAV;
pfMix_QuickLoad_RAW             Mix_QuickLoad_RAW;
pfMix_FreeChunk                 Mix_FreeChunk;
pfMix_FreeMusic                 Mix_FreeMusic;
pfMix_GetMusicType              Mix_GetMusicType;
pfMix_SetPostMix                Mix_SetPostMix;
pfMix_HookMusic                 Mix_HookMusic;
pfMix_HookMusicFinished         Mix_HookMusicFinished;
pfMix_GetMusicHookData          Mix_GetMusicHookData;
pfMix_ChannelFinished           Mix_ChannelFinished;
pfMix_RegisterEffect            Mix_RegisterEffect;
pfMix_UnregisterEffect          Mix_UnregisterEffect;
pfMix_UnregisterAllEffects      Mix_UnregisterAllEffects;
pfMix_SetPanning                Mix_SetPanning;
pfMix_SetPosition               Mix_SetPosition;
pfMix_SetDistance               Mix_SetDistance;
pfMix_SetReverseStereo          Mix_SetReverseStereo;
pfMix_ReserveChannels           Mix_ReserveChannels;
pfMix_GroupChannel              Mix_GroupChannel;
pfMix_GroupChannels             Mix_GroupChannels;
pfMix_GroupAvailable            Mix_GroupAvailable;
pfMix_GroupCount                Mix_GroupCount;
pfMix_GroupOldest               Mix_GroupOldest;
pfMix_GroupNewer                Mix_GroupNewer;
pfMix_PlayChannelTimed          Mix_PlayChannelTimed;
pfMix_PlayMusic                 Mix_PlayMusic;
pfMix_FadeInMusic               Mix_FadeInMusic;
pfMix_FadeInMusicPos            Mix_FadeInMusicPos;
pfMix_FadeInChannelTimed        Mix_FadeInChannelTimed;
pfMix_Volume                    Mix_Volume;
pfMix_VolumeChunk               Mix_VolumeChunk;
pfMix_VolumeMusic               Mix_VolumeMusic;
pfMix_HaltChannel               Mix_HaltChannel;
pfMix_HaltGroup                 Mix_HaltGroup;
pfMix_HaltMusic                 Mix_HaltMusic;
pfMix_ExpireChannel             Mix_ExpireChannel;
pfMix_FadeOutChannel            Mix_FadeOutChannel;
pfMix_FadeOutGroup              Mix_FadeOutGroup;
pfMix_FadeOutMusic              Mix_FadeOutMusic;
pfMix_FadingMusic               Mix_FadingMusic;
pfMix_FadingChannel             Mix_FadingChannel;
pfMix_Pause                     Mix_Pause;
pfMix_Resume                    Mix_Resume;
pfMix_Paused                    Mix_Paused;
pfMix_PauseMusic                Mix_PauseMusic;
pfMix_ResumeMusic               Mix_ResumeMusic;
pfMix_RewindMusic               Mix_RewindMusic;
pfMix_PausedMusic               Mix_PausedMusic;
pfMix_SetMusicPosition          Mix_SetMusicPosition;
pfMix_Playing                   Mix_Playing;
pfMix_PlayingMusic              Mix_PlayingMusic;
pfMix_SetMusicCMD               Mix_SetMusicCMD;
pfMix_SetSynchroValue           Mix_SetSynchroValue;
pfMix_GetSynchroValue           Mix_GetSynchroValue;
pfMix_GetChunk                  Mix_GetChunk;
pfMix_CloseAudio                Mix_CloseAudio;
}

//==============================================================================
// Loader
//==============================================================================

private void load(SharedLib lib)
{
    bindFunc(Mix_Linked_Version)("Mix_Linked_Version", lib);
    bindFunc(Mix_OpenAudio)("Mix_OpenAudio", lib);
    bindFunc(Mix_AllocateChannels)("Mix_AllocateChannels", lib);
    bindFunc(Mix_QuerySpec)("Mix_QuerySpec", lib);
    bindFunc(Mix_LoadWAV_RW)("Mix_LoadWAV_RW", lib);
    bindFunc(Mix_LoadMUS)("Mix_LoadMUS", lib);
    bindFunc(Mix_LoadMUS_RW)("Mix_LoadMUS_RW", lib);
    bindFunc(Mix_QuickLoad_WAV)("Mix_QuickLoad_WAV", lib);
    bindFunc(Mix_QuickLoad_RAW)("Mix_QuickLoad_RAW", lib);
    bindFunc(Mix_FreeChunk)("Mix_FreeChunk", lib);
    bindFunc(Mix_FreeMusic)("Mix_FreeMusic", lib);
    bindFunc(Mix_GetMusicType)("Mix_GetMusicType", lib);
    bindFunc(Mix_SetPostMix)("Mix_SetPostMix", lib);
    bindFunc(Mix_HookMusic)("Mix_HookMusic", lib);
    bindFunc(Mix_HookMusicFinished)("Mix_HookMusicFinished", lib);
    bindFunc(Mix_GetMusicHookData)("Mix_GetMusicHookData", lib);
    bindFunc(Mix_ChannelFinished)("Mix_ChannelFinished", lib);
    bindFunc(Mix_RegisterEffect)("Mix_RegisterEffect", lib);
    bindFunc(Mix_UnregisterEffect)("Mix_UnregisterEffect", lib);
    bindFunc(Mix_UnregisterAllEffects)("Mix_UnregisterAllEffects", lib);
    bindFunc(Mix_SetPanning)("Mix_SetPanning", lib);
    bindFunc(Mix_SetPosition)("Mix_SetPosition", lib);
    bindFunc(Mix_SetDistance)("Mix_SetDistance", lib);
    bindFunc(Mix_SetReverseStereo)("Mix_SetReverseStereo", lib);
    bindFunc(Mix_ReserveChannels)("Mix_ReserveChannels", lib);
    bindFunc(Mix_GroupChannel)("Mix_GroupChannel", lib);
    bindFunc(Mix_GroupChannels)("Mix_GroupChannels", lib);
    bindFunc(Mix_GroupAvailable)("Mix_GroupAvailable", lib);
    bindFunc(Mix_GroupCount)("Mix_GroupCount", lib);
    bindFunc(Mix_GroupOldest)("Mix_GroupOldest", lib);
    bindFunc(Mix_GroupNewer)("Mix_GroupNewer", lib);
    bindFunc(Mix_PlayChannelTimed)("Mix_PlayChannelTimed", lib);
    bindFunc(Mix_PlayMusic)("Mix_PlayMusic", lib);
    bindFunc(Mix_FadeInMusic)("Mix_FadeInMusic", lib);
    bindFunc(Mix_FadeInMusicPos)("Mix_FadeInMusicPos", lib);
    bindFunc(Mix_FadeInChannelTimed)("Mix_FadeInChannelTimed", lib);
    bindFunc(Mix_Volume)("Mix_Volume", lib);
    bindFunc(Mix_VolumeChunk)("Mix_VolumeChunk", lib);
    bindFunc(Mix_VolumeMusic)("Mix_VolumeMusic", lib);
    bindFunc(Mix_HaltChannel)("Mix_HaltChannel", lib);
    bindFunc(Mix_HaltGroup)("Mix_HaltGroup", lib);
    bindFunc(Mix_HaltMusic)("Mix_HaltMusic", lib);
    bindFunc(Mix_ExpireChannel)("Mix_ExpireChannel", lib);
    bindFunc(Mix_FadeOutChannel)("Mix_FadeOutChannel", lib);
    bindFunc(Mix_FadeOutGroup)("Mix_FadeOutGroup", lib);
    bindFunc(Mix_FadeOutMusic)("Mix_FadeOutMusic", lib);
    bindFunc(Mix_FadingMusic)("Mix_FadingMusic", lib);
    bindFunc(Mix_FadingChannel)("Mix_FadingChannel", lib);
    bindFunc(Mix_Pause)("Mix_Pause", lib);
    bindFunc(Mix_Resume)("Mix_Resume", lib);
    bindFunc(Mix_Paused)("Mix_Paused", lib);
    bindFunc(Mix_PauseMusic)("Mix_PauseMusic", lib);
    bindFunc(Mix_ResumeMusic)("Mix_ResumeMusic", lib);
    bindFunc(Mix_RewindMusic)("Mix_RewindMusic", lib);
    bindFunc(Mix_PausedMusic)("Mix_PausedMusic", lib);
    bindFunc(Mix_SetMusicPosition)("Mix_SetMusicPosition", lib);
    bindFunc(Mix_Playing)("Mix_Playing", lib);
    bindFunc(Mix_PlayingMusic)("Mix_PlayingMusic", lib);
    bindFunc(Mix_SetMusicCMD)("Mix_SetMusicCMD", lib);
    bindFunc(Mix_SetSynchroValue)("Mix_SetSynchroValue", lib);
    bindFunc(Mix_GetSynchroValue)("Mix_GetSynchroValue", lib);
    bindFunc(Mix_GetChunk)("Mix_GetChunk", lib);
    bindFunc(Mix_CloseAudio)("Mix_CloseAudio", lib);
}


GenericLoader DerelictSDLMixer;
static this() {
    DerelictSDLMixer.setup(
        "SDL_mixer.dll",
        "libSDL_mixer.so, libSDL_mixer-1.2.so, libSDL_mixer-1.2.so.0",
        "",
        &load
    );
}
