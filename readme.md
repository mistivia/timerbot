# Timerbot

Telegram timer bot

## Deploy

用stack构建，然后在项目目录中创建文件`tgtoken`，写入bot的token。

Build with stack, and create a `tgtok` file with a bot token in working direction.

## Usage

The command:

    /timer <minutes>

The minutes must be within a day. You can set at most 4096 timers.

The timers won't be written to disk. All timers will be lost when bot restarts.
