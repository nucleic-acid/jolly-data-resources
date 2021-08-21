library("tidyverse")

a <- seq(0, 4 * pi, length.out = 200)
c <- tibble(a = a)

sailplot <- c %>%
  mutate(
    b1 = sin(a + 1),
    b2 = 1.1^2 * sin(a + 1.5),
    b3 = 1.3^2 * sin(a + 2),
    b4 = 1.5^2 * sin(a + 2.5),
    b5 = 2.1^2 * sin(a + 4),
    b6 = 1.9^2 * sin(a + 3.5),
    b7 = 2.3^2 * sin(a + 4.5),
    b8 = 2.5^2 * sin(a + 5),
    b9 = 2.7^2 * sin(a + 5.5)
  ) %>%
  pivot_longer(-a, names_to = "b", values_to = "bval") %>%
  arrange(desc(b)) %>%
  ggplot(aes(x = a, y = bval, color = b)) +
  geom_point(size = 0.05, alpha = 0.4) +
  geom_point(aes(a, 1.7^2 * sin(a + 3)), alpha = 0.6, size = 0.1, color = "#1BC7DC") +
  scale_color_brewer(palette = "Greys") +
  theme_void() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")


ggsave("sailplot.png", plot = sailplot, height = 5, width = 10, units = "in", dpi = "retina")
ggsave("sailplot.pdf", plot = sailplot, height = 3, width = 9, units = "in")
