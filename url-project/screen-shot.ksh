#!/usr/bin/env ksh
url=$1
name=`echo $2 | sed 's/[\/.:]/_/g'`

file_path="/Users/fons/Data/twitter-project"

cmd="/Users/fons/Build/cutycapt/CutyCapt/CutyCapt.app/Contents/MacOS/CutyCapt --url=$url --out=$file_path/screen_shot_$name.jpeg &"
echo $cmd >/tmp/screen-shot.out
eval $cmd >>/tmp/screen-shot.out
pid=$!

sleep 10
running=`/bin/ps -p $pid | grep $pid`
echo "running $running" >>/tmp/screen-shot.out
if [[ -n $running ]]
then
    sleep 20
    running=`/bin/ps -p $pid | grep $pid`
    echo "running $running" >>/tmp/screen-shot.out
    if [[ -n $running ]]
    then
	sleep 30
    fi
fi

running=`/bin/ps -p $pid | grep $pid`
echo "running $running" >>/tmp/screen-shot.out

if [[ -n $running ]]
then
    echo "pid $pid is still running; going in for the kill...."
    kill -9 $pid
    cmd="cp $file_path/error-message.jpeg $file_path/thumbnail_$name.jpeg"
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out    
    cmd="cp $file_path/error-message.jpeg $file_path/cropped_$name.jpeg"
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out
    exit 20
fi

cmd="/opt/local/bin/convert -crop x800+0+0 $file_path/screen_shot_$name.jpeg $file_path/cropped_$name.jpeg"
echo $cmd >>/tmp/screen-shot.out
eval $cmd >>/tmp/screen-shot.out

cmd="/opt/local/bin/convert -thumbnail x400 $file_path/cropped_${name}.jpeg $file_path/thumbnail_$name.jpeg"
echo $cmd >>/tmp/screen-shot.out
eval $cmd >>/tmp/screen-shot.out

exit 0 