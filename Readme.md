This is a very simple R script that uses `twitteR` to collect tweets
and display some statistics derived from them. To use it, you'll need
to get a `consumer_key`, `consumer_secret`, `access_token`, and
`access_secret` from Twitter. Once you've put those in the obvious
place and changed the base file name (`base.name`) and `hashtag`
you're searching for, you should be all set.

The `score.sentiment()` was copied directly from
<http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment>. It
no longer seems to be available there, but there is a version
available at
<https://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/>.

The lexicon used for positive/negative sentiment scoring is from
<https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html>.

Enjoy!

