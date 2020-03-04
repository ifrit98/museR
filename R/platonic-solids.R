

tet <- tetrahedron <-
  structure(list(
    vertices = vertices(3, 3),
    edges = edges(3, 3),
    faces = faces(3, 3),
    p = 3,
    q = 3,
    h = 4,
    dihedral_angle = 70.53,
    defect = pi,
    solid_angle = acos(23 / 27),
    face_angle = pi,
    tan_theta_div_2 = 1 / sqrt(2)
  ),
  class = c("tetrahedron", "list"))


cube <-
  structure(
    list(
      vertices = vertices(4, 3),
      edges = edges(4, 3),
      faces = faces(4, 3),
      p = 4,
      q = 3,
      h = 6,
      dihedral_angle = 90,
      defect = pi / 2,
      solid_angle = pi / 2,
      face_angle = (2*pi) / 3,
      tan_theta_div_2 = 1
    ),
    class = c("cube", "list")
  )


oct <- octahedron <-
  structure(
    list(
      vertices = vertices(3, 4),
      edges = edges(3, 4),
      faces = faces(3, 4),
      p = 3,
      q = 4,
      h = 6,
      dihedral_angle = 109.47,
      defect = (2*pi) / 3,
      solid_angle = 4 * asin(1 / 3),
      face_angle = pi / 2,
      tan_theta_div_2 = sqrt(2)
    ),
    class = c("octahedron", "list")
  )


dod <- dodecahedron <-
  structure(
    list(
      vertices = vertices(5, 3),
      edges = edges(5, 3),
      faces = faces(5, 3),
      p = 5,
      q = 3,
      h = 10,
      dihedral_angle = 70.53,
      defect = pi / 5,
      solid_angle = acos(2 / 11),
      face_angle = pi / 3,
      tan_theta_div_2 = phi
    ),
    class = c("dodecahedron", "list")
  )


ico <- icosahedron <-
  structure(
    list(
      vertices = vertices(3, 5),
      edges = edges(3, 5),
      faces = faces(3, 5),
      p = 3, # Schlafli number
      q = 5, # Schlafli number
      h = 10, # Coxeter number,
      dihedral_angle = 70.53,
      defect = pi / 3,
      solid_angle = 2*pi - 5*asin(2 / 3),
      face_angle = pi / 5,
      tan_theta_div_2 = phi^2
    ),
    class = c("dodecahedron", "list")
  )

