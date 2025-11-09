#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
model_name=$(echo "$input" | jq -r '.model.display_name')
model_id=$(echo "$input" | jq -r '.model.id')
version=$(echo "$input" | jq -r '.version')

# ANSI escape codes
reset="\e[0m"
bold="\e[1m"
dim="\e[2m"
italic="\e[3m"
inverse="\e[7m"

# Colors
red="\e[31m"
green="\e[32m"
yellow="\e[33m"
blue="\e[34m"
cyan="\e[36m"

# Format version with dim styling
printf -v version "${dim}v%s${reset}" "$version"

# Determine model style based on model name/ID
if [[ "$model_name" =~ [Oo]pus || "$model_id" =~ opus ]]; then
    # Red background with black text for Opus models
    model_style="${inverse}${red}"
elif [[ "$model_name" =~ [Ss]onnet || "$model_id" =~ sonnet ]]; then
    # Blue background with black text for Sonnet models
    model_style="${inverse}${blue}"
else
    # Default inverse for other models
    model_style="${inverse}"
fi

# Format directory as parent/current with only current bold
parent_dir=$(dirname "$current_dir")
current_basename=$(basename "$current_dir")
parent_basename=$(basename "$parent_dir")
if [[ "$parent_basename" == "." || "$parent_basename" == "/" ]]; then
    printf -v dir_display "$bold%s$reset" "$current_basename"
else
    printf -v dir_display "$dim%s/$reset$bold%s$reset" "$parent_basename" "$current_basename"
fi

# Git information
git_info=""
if git -C "$current_dir" rev-parse --git-dir >/dev/null 2>&1; then
    cd "$current_dir"
    
    # Get branch name
    branch=$(git branch --show-current 2>/dev/null)
    if [[ -z "$branch" ]]; then
        branch="detached"
    fi
    
    # Get modified files count (tracked files with changes, staged files, deleted files)
    modified_count=$(git status --porcelain 2>/dev/null | grep -c '^[ MADRCU][ MADRCU]')
    modified_info=""
    if [[ "$modified_count" -gt 0 ]]; then
        printf -v modified_info " ${yellow}+%d${reset}" "$modified_count"  # Yellow for modified files
    fi

    # Get untracked files count
    untracked_count=$(git status --porcelain 2>/dev/null | grep -c '^??')
    untracked_info=""
    if [[ "$untracked_count" -gt 0 ]]; then
        printf -v untracked_info " ${cyan}…%d${reset}" "$untracked_count"  # Cyan for untracked files
    fi

    # Get ahead/behind counts
    ahead_behind=$(git rev-list --count --left-right @{upstream}...HEAD 2>/dev/null)
    if [[ $? -eq 0 && -n "$ahead_behind" ]]; then
        # Upstream configured, use it
        behind=$(echo "$ahead_behind" | cut -f1)
        ahead=$(echo "$ahead_behind" | cut -f2)

        status_parts=""
        if [[ "$behind" -gt 0 ]]; then
            printf -v behind_part " ${green}↓%d${reset}" "$behind"  # Green for pulls needed
            status_parts+="$behind_part"
        fi
        if [[ "$ahead" -gt 0 ]]; then
            printf -v ahead_part " ${red}↑%d${reset}" "$ahead"   # Red for commits ahead
            status_parts+="$ahead_part"
        fi

        git_info=" [$branch$status_parts$modified_info$untracked_info]"
    else
        # No upstream configured, try origin/<current-branch>
        if git rev-parse --verify "origin/$branch" >/dev/null 2>&1; then
            ahead_behind=$(git rev-list --count --left-right "origin/$branch"...HEAD 2>/dev/null)
            if [[ $? -eq 0 && -n "$ahead_behind" ]]; then
                behind=$(echo "$ahead_behind" | cut -f1)
                ahead=$(echo "$ahead_behind" | cut -f2)

                status_parts=""
                if [[ "$behind" -gt 0 ]]; then
                    printf -v behind_part " ${green}↓%d${reset}" "$behind"  # Green for pulls needed
                    status_parts+="$behind_part"
                fi
                if [[ "$ahead" -gt 0 ]]; then
                    printf -v ahead_part " ${red}↑%d${reset}" "$ahead"   # Red for commits ahead
                    status_parts+="$ahead_part"
                fi

                git_info=" [$branch$status_parts$modified_info$untracked_info]"
            else
                git_info=" [$branch$modified_info$untracked_info]"
            fi
        else
            # No origin/<branch> either, just show branch name
            git_info=" [$branch$modified_info$untracked_info]"
        fi
    fi
fi

# Output the status line with proper escape sequence interpretation
printf "%s %b %s %b %b%s" "$version" "$model_style" "$model_name" "$reset" "$dir_display" "$git_info"
