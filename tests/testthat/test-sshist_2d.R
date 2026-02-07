test_that("sshist_2d correctly handles discrete integer data (comb effect protection)", {
  # Загружаем тестовые данные (X - float, Y - int)
  # Файл должен лежать в папке тестов или генерироваться на лету
  # Для надежности сгенерируем похожие данные прямо тут,
  # чтобы тест не зависел от внешнего CSV файла
  set.seed(42)
  # X: смесь нормальных, float
  x <- c(rnorm(500, -1, 1), rnorm(1000, 1, 0.5))
  # Y: смесь нормальных, но ОКРУГЛЕННАЯ до целых (имитация данных из CSV)
  y_raw <- c(rnorm(500, 55, 5), rnorm(1000, 75, 8))
  y <- round(y_raw)

  # Запускаем алгоритм с запросом на 100 бинов
  res <- sshist_2d(x, y, n_max = 100)

  # --- ПРОВЕРКИ ---

  expect_s3_class(res, "sshist_2d")

  # 1. Проверка оси X (Float)
  # Тут разрешение высокое, ограничений быть не должно (кроме n_max)
  # Python дал 3 бина. Проверим, что R находит что-то разумное (не 1 и не 100)
  expect_gt(res$opt_nx, 1)
  expect_lt(res$opt_nx, 50)

  # 2. Проверка оси Y (Integer) - САМОЕ ВАЖНОЕ
  # Диапазон данных Y примерно 60 (от ~40 до ~100). Разрешение 1.
  # Максимально возможное число бинов = Range / 1 ≈ 60.
  # Если бы защиты не было, алгоритм мог бы выбрать 100.
  # Мы ожидаем, что алгоритм САМ ограничил поиск.

  y_range <- max(y) - min(y)
  res_y <- 1 # мы знаем, что подали целые числа
  theoretical_limit <- floor(y_range / res_y)

  # Проверяем, что R не вышел за физический предел разрешения
  expect_lte(res$opt_ny, theoretical_limit)

  # Проверяем, что в tested векторе нет значений выше лимита
  expect_lte(max(res$ny_tested), theoretical_limit)

  # 3. Проверка формы результата
  expect_equal(dim(res$cost_matrix), c(length(res$nx_tested), length(res$ny_tested)))
})

test_that("sshist_2d runs on external csv data", {
    # Предполагаем, что в CSV: V1 - X, V2 - Y
    df <- read.csv("test_data_2d.csv", header = FALSE)

    # Запускаем
    res <- sshist_2d(df$V1, df$V2)

    # Наша реализация должна дать меньше, так как Y - целые числа.
    expect_equal(res$opt_ny, 9)
    expect_equal(res$opt_nx, 20)

    # Проверяем, что матрица стоимости посчитана
    expect_false(any(is.na(res$cost_matrix)))
})
