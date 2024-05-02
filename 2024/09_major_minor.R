library(spotifyr)
library(tidyverse)
library(tidylog)
#library(visdat)
library(janitor)
library(ggtext)

# example code & analysis
# https://frie.codes/posts/shuffling-spotify-liked-songs-checking-assumptions/

# sets access token for session - keys in renviron
# no need to do this
# access_token <- get_spotify_access_token()



# read api documentation
scopes = c(
	"user-library-read",
	"user-read-recently-played",
	"playlist-read-private",
	"playlist-read-collaborative",
	"user-read-private"
)

auth <- spotifyr::get_spotify_authorization_code(Sys.getenv("SPOTIFY_CLIENT_ID"),
																								Sys.getenv("SPOTIFY_CLIENT_SECRET"),
																								scopes)

playlists <- spotifyr::get_user_playlists('dannebrog13', limit = 20,
																				 offset = 0, authorization = auth)


my_id <- 'dannebrog13'
my_plists <-  get_user_playlists('dannebrog13', limit = 50, offset = 0,
																 authorization = get_spotify_authorization_code(),
																 include_meta_info = FALSE)

my_plists <- my_plists %>%
	filter(owner.display_name == "dannebrog13")

my_plists %>%
	count(name)

# id for discover weekly & archives and liked
dwid <- "37i9dQZEVXcO630jAtRzBd"
dwaid <- "5bnY1Oc9Lb1cWHFTA5uahV"
dwa2id <- "18Wp6JmLlljDY2T6M1Sda4"
likeddid <- "6mlrhYRlFv3Ji5xwzqFWwQ"

# get tracks for discover weekly archive - this will only get 100, per API limit
# dwa_tracks <- get_playlist_tracks(dwaid, fields = NULL, limit = 100,
#                     offset = 0, market = NULL,
#                     authorization = get_spotify_access_token(),
#                     include_meta_info = FALSE)

# gets all audio features of all tracks in discover weekly playlist
dwa_feat <-
	get_playlist_audio_features(my_id, dwaid,
															authorization = auth)
glimpse(dwa_feat)
write_rds(dwa_feat, 'all_dwa_feat.rds')

# script for this from https://www.rpubs.com/womeimingzi11/how_my_spotify_looks_like
all_dwa_tracks <-
	ceiling(get_playlist_tracks(dwaid, include_meta_info = TRUE)[['total']] / 100) %>%
	seq() %>%
	map(function(x) {
		get_playlist_tracks(dwaid, fields = NULL, limit = 100, offset = (x - 1) * 100,
												market = NULL,
												authorization = get_spotify_access_token(),
												include_meta_info = FALSE)
	}) %>% reduce(rbind)

write_rds(all_dwa_tracks, 'all_dwa_tracks.rds')
glimpse(all_dwa_tracks)

all_liked_tracks <-
	ceiling(get_playlist_tracks("6mlrhYRlFv3Ji5xwzqFWwQ", include_meta_info = TRUE)[['total']] / 50) %>%
	seq() %>%
	map(function(x) {
		get_playlist_tracks("6mlrhYRlFv3Ji5xwzqFWwQ",
												fields = NULL,
												limit = 50, offset = (x - 1) * 50,
												market = NULL,
												authorization = get_spotify_access_token(),
												include_meta_info = FALSE)
	}) %>% reduce(rbind)

view(all_liked_tracks)

write_rds(all_liked_tracks, '~/Data/r/spotify-analysis/data/liked_songs_list.rds')

glimpse(all_liked_tracks)
getwd()


## tibble for liked tracks ids to use in function.
liked_tracks_ids <- as_tibble(all_liked_tracks$track.id) %>%
	rename(track_id = value)
glimpse(liked_tracks_ids)


# code for function
# from https://github.com/charlie86/spotifyr/issues/130#issuecomment-885076286

id <- unique(all_liked_tracks$track.id)

set.seed(1, sample.kind = "Rounding")

id_s <- sample(id, replace = F)

# Generating an access token to use Spotify’s API
token <- get_spotify_access_token(Sys.getenv("SPOTIFY_CLIENT_ID"),
																	Sys.getenv("SPOTIFY_CLIENT_SECRET"))

Feat_scraper <- function(x) {
	# omitting progress info
	base::options(warn =-1)
	# assigning length of an ID vector to a proxy object
	entire <- length(x)
	# setting seed for repo purposes
	set.seed(1, sample.kind = "Rounding")
	# assigning 100 sampled IDs to a vector to account for Spotify's limit
	v1a <- as.character(sample(x, 100, replace = F))
	# assigning a tibble with features of those 100 IDs. This tibble will be
	# extended below.
	tib <- spotifyr::get_track_audio_features(v1a, token)
	# replacing any IDs with new ones if those IDs are already in the tibble
	if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
	# creating a while loop on the condition that the rows of the tibble are
	# less and/or equal to the length of the entire object
	while (nrow(tib) <= entire) {
		# Setting seed for repo purposes
		set.seed(42, sample.kind = "Rounding")
		# assigning 100 sampled IDs from the new IDs from above to a base vector
		# according to Spotify's limit as long as the object IDs are greater
		# than 100. If the remaining IDs are less than 100, these remaining IDs
		# will be sampled.
		v1b <- as.character(sample(x, ifelse(length(x) > 100, 100, length(x)),
															 replace = F))
		# extending the tibble from above to create a complete tibble with all
		# retrieved audio features of all track IDs of the object in question
		tib %<>% full_join(spotifyr::get_track_audio_features(v1b,token),
											 by = c("danceability", "energy", "key", "loudness",
											 			 "mode", "speechiness", "acousticness",
											 			 "instrumentalness", "liveness",
											 			 "valence", "tempo", "type", "id", "uri",
											 			 "track_href", "analysis_url", "duration_ms",
											 			 "time_signature"))
		# replacing any IDs with new ones if those IDs are already in the tibble
		if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
		# If the rows of the tibble are equal to the length of the entire object
		# in question…,
		if (nrow(tib) == entire)
			#…break the loop.
			break
	}
	# outputting the entire tibble
	return(tib)
}


# run these three in order to calculate time it took
start <- Sys.time()
# names the df to be produced by feat_scraper function
liked_track_features <- Feat_scraper(id_s)
end <- Sys.time()

# calculates how long it took
process <- end-start
print(process)

glimpse(liked_track_features)
glimpse(all_liked_tracks)

liked_songs1 <- liked_track_features %>%
	mutate(key_name = case_when(key == 0 ~ "C", key == 1 ~ "C#/Db", key == 2 ~ "D",
															key == 3 ~ "D#/Eb", key == 4 ~ "E", key == 5 ~ "F",
															key == 6 ~ "F#/Gb", key == 7 ~ "G", key == 8 ~ "G#/Ab",
															key == 9 ~ "A", key == 10 ~ "A#/Bb", key == 11 ~ "B")) %>%
	mutate(mode_name = case_when(mode == 0 ~ "Minor", mode == 1 ~ "Major")) %>%
	mutate(key_mode = paste(key_name, mode_name, sep = " ")) %>%
	select(track.id = id, key_name:key_mode, time_signature, tempo, duration_ms,
				 danceability, energy, loudness, speechiness:valence,
				 key, mode, type, uri, track_href, analysis_url) %>%
	left_join(all_liked_tracks) %>%
	select(-video_thumbnail.url, -track.episode, -added_by.href:-added_by.external_urls.spotify) %>%
	filter(!track.id == "05F8wRJJVkvT8CnFVSgflf")

glimpse(liked_songs1)

liked_songs <- unnest_wider(liked_songs1, track.artists,
																		names_sep = "_") %>%
	select(-track.artists_external_urls.spotify, -track.artists_uri, -track.artists_type, -track.artists_href) %>%
	mutate(track.artist_names = map_chr(track.artists_name, toString)) %>%
	separate(track.artist_names, paste0('track.artist', c(1:6)), sep = ',', remove = F) %>%
	mutate(across(56:60, str_trim)) %>%
	select(track.id:track.artists_id, track.artist_names:track.artist6, everything(), -track.artists_name)

glimpse(liked_songs)

write_rds(liked_songs, '~/Data/r/spotify-analysis/data/liked_songs.rds')


glimpse(liked_songs_artists)

liked_songs <- readRDS(file = "~/Data/r/spotify-analysis/data/liked_songs.rds")

liked_songs %>%
	count(track.artist1) %>%
	arrange(desc(n)) %>%
	view()

## visualization
# bar chart for pct major/minor, pct by key, pct by key/mode combo


liked_songs %>%
	group_by(key_name, mode_name) %>%
	summarise(key_mode_n = n())
	group_by(mode_name) %>%
	summarise(mode_n = n()) %>%
	mutate(mode_pct = mode_n /sum(mode_n)) %>%
	ungroup()

liked_songs %>%
	select(mode_name, track.popularity) %>%
	group_by(mode_name) %>%
	mutate(popular_mean = mean(track.popularity)) %>%
	view()

liked_songs %>%
	group_by(key_mode) %>%
	mutate(pop_mean = mean(track.popularity)) %>%
	select(key_mode, pop_mean) %>%
	distinct(key_mode, .keep_all = TRUE) %>%
	ggplot(aes(key_mode, pop_mean)) +
	geom_bar(stat = "identity")

ggplot(data = liked_songs, aes(x = valence, y = energy, color = mode_name)) +
	#	geom_jitter() +
	geom_point() +
	geom_smooth() +
	geom_vline(xintercept = 0.5) +
	geom_hline(yintercept = 0.5)

ggplot(data = liked_songs, aes(x = valence, y = energy)) +
	#	geom_jitter() +
	geom_point() +
	geom_vline(xintercept = 0.5) +
	geom_hline(yintercept = 0.5) +
	geom_smooth() +
	facet_wrap(~ mode_name)


glimpse(liked_songs)
liked_songs_summary <-
liked_songs %>%
	rename(duration.ms = duration_ms) %>%
	# change loudness to abs value for better scaling in plots
	mutate(loudness = abs(loudness)) %>%
	group_by(mode_name) %>%
	summarise_at(vars(tempo:valence),
							 						 list(mean = mean,
							 		 q25 = ~quantile(., 0.25),
							 		 med = median,
							 		 q75 = ~quantile(., 0.75),
							 	min = min, max = max)) %>%
 	pivot_longer(-mode_name,
 							 names_to = "var_measure",
 							 values_to = "value") %>%
	separate_wider_delim(var_measure, "_", names = c("var", "measure")) %>%
	#mutate(value = round(value, 2)) %>%
	# change duration to seconds for easier explanation
	mutate(value = ifelse(var == "duration.ms", value / 1000, value)) %>%
	mutate(value = round(value, 2)) %>%
	mutate(var = ifelse(var == "duration.ms", "duration_sec", var))
## factoring actually not good for later needs
# %>%
# 	mutate(var = factor(var,
# 											levels = c("acousticness", "danceability", "energy", "instrumentalness",
# 																 "liveness", "speechiness", "valence",
# 																 "tempo", "duration_sec", "loudness")))

glimpse(liked_songs_summary)

# conditional scale_x to account for different limits in values
# scale limits: acousticness, danceability, energy, instrumentalness, liveness speechiness valence 0-1,
# loudness (in dbs) -60bb to 0 (abs value used here for scaling)
# max tempo 210, min 60, duration max 1100, min 60
# write scale as object



# spotify defined features in facet

liked_songs_summary %>%
	filter(var %in% c("acousticness", "danceability", "energy", "instrumentalness",
									 "liveness", "speechiness", "valence")) %>%
	pivot_wider(names_from = measure, values_from = value) %>%
	ggplot() +
	geom_segment(aes(x= min, xend=max, y=fct_rev(mode_name), yend=fct_rev(mode_name)), color="grey") +
	geom_point( aes(x=min, y=mode_name), color="#4E79A7", size=3 ) +
	geom_point( aes(x=max, y=mode_name), color="#79A74E", size=3 ) +
	geom_point( aes(x=mean, y=mode_name), color="#A74E79", size=3 ) +
	scale_x_continuous(limits = c(0, 1), breaks = scales::pretty_breaks(4)) +
	labs(x = "", y = "", title = "Spotify defined audio feature values for Liked Songs playlist by major & minor key tracks",
			 subtitle = "<span style='color: #4E79A7;'>Min</span> \n<span style='color: #A74E79;'>Mean</span> \n<span style='color: #79A74E;'>Max</span>",
			 caption = "*Data from Spotify API via spotifyr package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.subtitle = element_markdown(size = 12),
				plot.caption = element_markdown(),
				strip.text.x = element_text(size = 10 )) +
	facet_wrap(~ var, ncol = 2, scales = "free_y")


# var == "tempo" ~ c(60, 210),
liked_songs_summary %>%
	filter(var == "tempo") %>%
	select(-var) %>%
	pivot_wider(names_from = measure, values_from = value) %>%
	ggplot() +
	geom_segment(aes(x= min, xend=max, y=fct_rev(mode_name), yend=fct_rev(mode_name)), color="grey") +
	geom_point( aes(x=min, y=mode_name), color="#4E79A7", size=3 ) +
	geom_point( aes(x=max, y=mode_name), color="#79A74E", size=3 ) +
	geom_point( aes(x=mean, y=mode_name), color="#A74E79", size=3 ) +
	scale_x_continuous(limits = c(0, 220), breaks = scales::pretty_breaks(4)) +
	labs(x = "", y = "", title = "Minimal difference in tempo values between major & minor key songs",
			 subtitle = "*Audio feature: tempo (beats per minute) <br>
			 x axis scale reflects min & max values for playlist.*",
			 caption = "*Data from Spotify API via spotifyr package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.subtitle = element_markdown(size = 12),
				plot.caption = element_markdown(),
				strip.text.x = element_text(size = 10 ))

# var == "duration" ~ c(60, 1100))
#duration <-
liked_songs_summary %>%
	filter(var == "duration_sec") %>%
	select(-var) %>%
	pivot_wider(names_from = measure, values_from = value) %>%
	ggplot() +
	geom_segment(aes(x= min, xend=max, y=fct_rev(mode_name), yend=fct_rev(mode_name)), color="grey") +
	geom_point( aes(x=min, y=mode_name), color="#4E79A7", size=3 ) +
	geom_point( aes(x=max, y=mode_name), color="#79A74E", size=3 ) +
	geom_point( aes(x=mean, y=mode_name), color="#A74E79", size=3 ) +
	scale_x_continuous(limits = c(0, 1100), breaks = scales::pretty_breaks(5)) +
	labs(x = "", y = "", title = "Minimal difference in average length, though longest song is major key.",
			 subtitle = "*Audio feature: duration (in seconds)*",
			 caption = "*Data from Spotify API via spotifyr package*") +
	annotate(geom = "richtext",
					 label = "<span style='color: #4E79A7;'>Min</span> \n<span style='color: #A74E79;'>Mean</span> \n<span style='color: #79A74E;'>Max</span>",
					 x = 700, y = 1.5) +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.subtitle = element_markdown(),
				plot.caption = element_markdown())


# var == "loudness" ~ c(0, 30),
#loud <-
liked_songs_summary %>%
	filter(var == "loudness") %>%
	select(-var) %>%
	pivot_wider(names_from = measure, values_from = value) %>%
	ggplot() +
	geom_segment(aes(x= min, xend=max, y=fct_rev(mode_name), yend=fct_rev(mode_name)), color="grey") +
	geom_point( aes(x=min, y=mode_name), color="#4E79A7", size=3 ) +
	geom_point( aes(x=max, y=mode_name), color="#79A74E", size=3 ) +
	geom_point( aes(x=mean, y=mode_name), color="#A74E79", size=3 ) +
	scale_x_continuous(limits = c(0, 60)) +
	labs(x = "", y = "", title = "Minimal difference in loudness profile between major & minor key songs",
			 subtitle = "*Audio feature: loudness. <br>Original values are -60db-0db (decibels). Absolute value use for scaling.*",
			 caption = "*Data from Spotify API via spotifyr package*") +
	annotate(geom = "richtext",
					 label = "<span style='color: #4E79A7;'>Min</span> \n<span style='color: #A74E79;'>Mean</span> \n<span style='color: #79A74E;'>Max</span>",
					 x = 30, y = 1.5) +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.subtitle = element_markdown(),
				plot.caption = element_markdown())



## code not needed

# mybarplot <- function(mydf, myxcol, myycol, mytitle) {
# 	ggplot(data = mydf, aes(x=reorder(myxcol, myycol), y=myycol)) +
# 		geom_col(color = "black", fill="#0072B2") +
# 		xlab("") +
# 		ylab("") +
# 		coord_flip() +
# 		ggtitle(mytitle) +
# 		theme_classic()   +
# 		theme(plot.title=element_text(size=24))
# }
# mybarplot(bos_values, RegionName, Zhvi,
# 					"Zillow Home Value Index by Boston Neighborhood")

# y is mode_name, ask for x & title input, no need for fill or color
# basic plot plus theme, allow x axis scale to be modified after
# song_feature_plot <- function(xvar plot_title) {
# 	g <-
# 		ggplot()
#
#
#
# 	return(g)
# }
#
# song_feature_plot()
#
# gets just the artist info after
# liked_songs_artists <- liked_songs %>%
# 	select(track.artists) %>%
# 	reduce(rbind) %>%
# 	reduce(rbind)


# audio_analysis1 <- get_track_audio_analysis("5Fe95ds8sn5cGyv2XJNBxG",
# 												 authorization = get_spotify_access_token())

# xlims <- if (max(liked_songs_summary$value) >= 500) c(0, 5000) else c(0, 500)
# xlims <- if (max(VOORTOOJOUD$INIMEST) >= 500) c(0, 1) else c(0, 500)
#
# xlims <- case_when(liked_songs_summary$var %in% c("acousticness", "danceability", "energy", "instrumentalness",
# 																									"liveness", "loudness", "speechiness", "valence") ~ c(0,1),
# 									 liked_songs_summary$var == "loudness" ~ c(0, 30),
# 									 liked_songs_summary$var == "tempo" ~ c(60, 210),
# 									 liked_songs_summary$var == "duration" ~ c(60, 1100))
#
# xlims <- if (liked_songs_summary$var %in%
# 						 c("acousticness", "danceability", "energy", "instrumentalness",
# 						 	"liveness", "loudness", "speechiness", "valence")) c(0, 1)
# else c(0, 1100)
#
# var == "loudness" ~ c(0, 30),
# var == "tempo" ~ c(60, 210),
# var == "duration" ~ c(60, 1100))
#
# scale_x_continuous(limits = ifelse(c(TRUE, max(VOORTOOJOUD$INIMEST) >= 500), c(0, 5000), c(0, 500)))
#
#
# my_breaks <- function(x) { if (max(x) < 6000) seq(0, 5000, 1000) else seq(0, 15000, 5000) }
#
#
# data_frame$col3 = ifelse(data_frame$col1>4,"cond1 satisfied",
# 												 ifelse(data_frame$col2 %in% c("A","C"),
# 												 			 "cond2 satisfied",
# 												 			 "both failed"))
#
# scale_x_continuous(limits = ifelse(c(TRUE, max(liked_songs_summary$value) > 1), c(0, 1100), c(0, 1)))
#
# xlims <- ifelse(c(TRUE,
# 									liked_songs_summary$var == "acousticness"), c(0,1), c(0, 100))
#
# xlims <- ifelse(c(TRUE,
# 									liked_songs_summary$var %in% c("acousticness", "danceability", "energy", "instrumentalness",
# 																								 "liveness", "speechiness", "valence")), c(0,1), c(0, 1))
#
# ifelse(liked_songs_summary$var == "loudness", c(0, 30),
# 			 ifelse(liked_songs_summary$var == "tempo", c(60, 210), c(60, 1100))))
#
# xlims <- function(x) {
# 	ifelse(liked_songs_summary$var %in% c("acousticness", "danceability", "energy", "instrumentalness",
# 																				"liveness", "speechiness", "valence"), c(0,1),
# 				 ifelse(liked_songs_summary$var == "loudness", c(0, 30),
# 				 			 ifelse(liked_songs_summary$var == "tempo", c(60, 210), c(60, 1100))))
# }
#
# my_breaks <- function(x) { if (max(x) <=1) seq(0, 1, .25) else seq(0, 1100, 250) }
#
# ggplot(data = liked_songs_summary %>% filter(measure == "mean"),
# 			 (aes(x = value, y = mode_name))) +
# 	geom_bar(stat = "identity") +
# 	facet_wrap(~ var, scales = "free", ncol = 5) +
# 	scale_x_continuous(breaks = my_breaks)
#
# scale_x_continuous(limits = ifelse(max(liked_songs_summary$value) > 1), c(0, 1100), c(0, 1)))

# scale_x_continuous(limits = xlims)
# scale_x_continuous(breaks = scales::pretty_breaks(4), limits = c(0, NA))

# ggplot(data = liked_songs_summary %>% filter(measure == "mean"),
# 			 (aes(x = mode_name, y = value))) +
# 	geom_point() +
# 	coord_flip() +
# 	facet_wrap(~ var, scales = "free")

# ggplot(data = liked_songs_summary %>%
# 			 	filter(measure == "mean", var == "loudness"),
# 			 (aes(x = value, y = mode_name, fill = mode_name))) +
# 	geom_bar(stat = "identity") +
# 	geom_text(data = liked_songs_summary %>%
# 							filter(measure == "mean", var == "loudness"),
# 						aes(label = value, color = mode_name),
# 						position = position_stack(),
# 						hjust = -.25, size = 6) +
# 	scale_x_continuous(limits = c(0, 30)) +
# 	labs(x = "", y = "", title = "Audio feature: loudness",
# 			 subtitle = "*Original values are -60db-0db (decibels). Absolute value use for scaling.*",
# 			 caption = "*Data from Spotify API via spotifyr package*") +
# 	theme_minimal() +
# 	theme(panel.grid = element_blank(), legend.position = "none",
# 				plot.subtitle = element_markdown(),
# 				plot.caption = element_markdown())
#
# ggplot(data = liked_songs_summary %>%
# 			 	filter(measure == "mean", var == "tempo"),
# 			 (aes(x = value, y = mode_name, fill = mode_name))) +
# 	geom_bar(stat = "identity") +
# 	# geom_text(data = liked_songs_summary %>%
# 	# 						filter(measure == "mean", var == "tempo"),
# 	# 					aes(label = value, color = mode_name),
# 	# 					position = position_stack(),
# 	# 					hjust = -.25, size = 6) +
# 	scale_x_continuous(limits = c(0, 200)) +
# 	labs(x = "", y = "", title = "Audio feature: tempo (beats per minute)",
# 			 subtitle = "*x axis scale reflects min & max values for playlist.*",
# 			 caption = "*Data from Spotify API via spotifyr package*") +
# 	theme_minimal() +
# 	theme(panel.grid = element_blank(), legend.position = "none",
# 				plot.subtitle = element_markdown(),
# 				plot.caption = element_markdown())
#
# ggplot(data = liked_songs_summary %>%
# 			 	filter(measure == "mean", var %in% c("acousticness", "danceability", "energy", "instrumentalness",
# 			 																			 "liveness", "speechiness", "valence")),
# 			 (aes(x = value, y = mode_name, fill = mode_name))) +
# 	geom_bar(stat = "identity") +
# 	geom_text(aes(label = value, color = mode_name),
# 						position = position_stack(),
# 						hjust = -.25, size = 6) +
# 	scale_x_continuous(limits = c(0, 1)) +
# 	labs(x = "", y = "",
# 			 title = "Minimal difference in average audio feature values between major & minor key songs.",
# 			 subtitle = "*Audio features with scale 0-1*",
# 			 caption = "*Data from Spotify API via spotifyr package*") +
# 	theme_minimal() +
# 	theme(panel.grid = element_blank(), legend.position = "none",
# 				plot.subtitle = element_markdown(),
# 				plot.caption = element_markdown(),
# 				strip.text.x = element_text(size = 10 )) +
# 	facet_wrap(~ var, ncol = 2, scales = "free")

#{. ->> tmp} %>%
