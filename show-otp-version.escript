#!/usr/bin/env escript
%% -*- mode:erlang -*-
%%! -smp enable -sname show_otp_release

%%% The MIT License
%%%
%%% Copyright (C) 2015 by Tatsuya Kawano
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

main([]) ->
    OTPRelease = erlang:system_info(otp_release),
    case re:run(OTPRelease, "^R1\\d") of
        {match, _} ->
            %% In R16 or earlier, otp_release has both the major and minor versions.
            io:format(OTPRelease);
        nomatch ->
            %% In 17 or newer, otp_release only has the major version. So get the full
            %% version string from OTP_VERSION file. For further details, see
            %% http://www.erlang.org/doc/system_principles/versions.html
            Path = filename:join([code:root_dir(), "releases", OTPRelease, "OTP_VERSION"]),
            case file:open(Path, [read]) of
                {ok, IODevice} ->
                    case file:read_line(IODevice) of
                        {ok, Line} ->
                            io:format(string:strip(Line, right, $\n));
                        Err ->  %% including 'eof'
                            print_error(Path, Err)
                    end,
                    _ = file:close(IODevice);
                Err ->
                    print_error(Path, Err)
            end
    end.

print_error(Path, Err) ->
    io:format("Can't read OTP_VERSION file.~n  path: ~p~n  reason: ~p~n", [Path, Err]).
