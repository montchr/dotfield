#!/usr/bin/env bash

TIMEOUT=1500

get_volume() {
    local volume
    volume="$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $2}')"
    echo "$volume * 100 / 1" | bc
}

is_muted() {
    [[ $(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print $3}') == "[MUTED]" ]]
}

get_brightness() {
    local cur max
    cur="$(brightnessctl get)"
    max="$(brightnessctl max)"
    echo "scale = 2; quo = $cur / $max; scale = 0; quo * 100 / 1;" | bc
}

notify_audio() {
    dunstify -h string:x-canonical-private-synchronous:audio "$@"
}

notify_audio_volume() {
    local volume label
    volume=$1
    label="[ volume: $volume% ]"

    if [[ $volume -eq 0 ]] || is_muted; then
        notify_audio "[ MUTED ]" -h int:value:"$volume" -t "$TIMEOUT" --icon audio-volume-muted
    elif [[ $volume -le 30 ]]; then
        notify_audio "$label" -h int:value:"$volume" -t "$TIMEOUT" --icon audio-volume-low
    elif [[ $volume -le 70 ]]; then
        notify_audio "$label" -h int:value:"$volume" -t "$TIMEOUT" --icon audio-volume-medium
    else
        notify_audio "$label" -h int:value:"$volume" -t "$TIMEOUT" --icon audio-volume-high
    fi
}

notify_brightness() {
    dunstify -h string:x-canonical-private-synchronous:brightness "$@"
}

notify_brightness_level() {
    local level label
    level=$1
    label="[ brightness: $level% ]"

    if [[ $level -eq 0 ]]; then
        notify_brightness "$label" -h int:value:"$level" -t "$TIMEOUT" --icon display-brightness-off-symbolic
    elif [[ $level -le 30 ]]; then
        notify_brightness "$label" -h int:value:"$level" -t "$TIMEOUT" --icon display-brightness-low-symbolic
    elif [[ $level -le 70 ]]; then
        notify_brightness "$label" -h int:value:"$level" -t "$TIMEOUT" --icon display-brightness-medium-symbolic
    else
        notify_brightness "$label" -h int:value:"$level" -t "$TIMEOUT" --icon display-brightness-high-symbolic
    fi
}

main() {
    case "$1" in
        audio) notify_audio_volume "$(get_volume)" ;;
        brightness) notify_brightness_level "$(get_brightness)" ;;
        *)
            echo "Invalid arguments:"
            echo "$1"
            exit 2
    esac
}

main "$@"
