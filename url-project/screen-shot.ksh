#!/usr/bin/env bash
url=$1
name=`echo $2 | sed 's/[\/.:]/_/g'`

if [[ -z $url ]]
then
    echo "no url specified"
    exit 1
fi

if [[ -z $name ]]
then
    echo "no name specified $url $name"
    exit 1
fi

file_path="${HOME}/Data/twitter-project"
if [[ ! -d $file_path ]]
then
    echo "data directory $file_path does not exist"
    echo "that's where all the images go..."
    exit 1
fi

cmd_path="${HOME}/Build"
app=`find $cmd_path -type f -name CutyCapt`
if [[ ! -x "${app}" ]]
then
    echo "unable to locate CutyCapt..."
    echo "was looking for it somewhere below here : $cmd_path"
    exit 1
fi

convert_app=`which convert`
if [[ ! -x $convert_app ]]
then
    echo "unable to find convert (which is an image converter)"
    echo "it's part of the ImageMagick suite"
    exit 1
fi

cmd="$app --url='$url' --out=$file_path/screen_shot_$name.jpeg > /tmp/cutyapp.out 2>&1 &"
echo `date` $cmd >/tmp/screen-shot.out
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
    echo "pid $pid is still running; going in for the kill...." >>/tmp/screen-shot.out
    kill -9 $pid
    cmd="cp $file_path/error-message.jpeg $file_path/thumbnail_$name.jpeg" 
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out    
    cmd="cp $file_path/error-message.jpeg $file_path/cropped_$name.jpeg"
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out
    cat /tmp/cutyapp.out >> /tmp/screen-shot.out
    exit 20
fi

if [[ ! -e "$file_path/screen_shot_$name.jpeg" ]]
then
    echo "no output file generated [$file_path/screen_shot_$name.jpeg]; copying over generic error messages" >>/tmp/screen-shot.out
    cmd="cp $file_path/error-message.jpeg $file_path/thumbnail_$name.jpeg" 
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out    
    cmd="cp $file_path/error-message.jpeg $file_path/cropped_$name.jpeg"
    echo $cmd >>/tmp/screen-shot.out
    eval $cmd >>/tmp/screen-shot.out
    cat /tmp/cutyapp.out >> /tmp/screen-shot.out
    exit 30
fi

cmd="$convert_app -crop x800+0+0 $file_path/screen_shot_$name.jpeg $file_path/cropped_$name.jpeg"
echo $cmd >>/tmp/screen-shot.out
eval $cmd >>/tmp/screen-shot.out

cmd="$convert_app -thumbnail x400 $file_path/cropped_${name}.jpeg $file_path/thumbnail_$name.jpeg"
echo $cmd >>/tmp/screen-shot.out
eval $cmd >>/tmp/screen-shot.out
cat /tmp/cutyapp.out >> /tmp/screen-shot.out
exit 0 
