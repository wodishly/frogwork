#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;

bool isOnThickCircle(vec2 s, vec2 navel, float inner, float outer) {
  return inner < distance(s, navel) && distance(s, navel) < outer;
}

bool isInCircle(vec2 s, vec2 navel, float arm) {
  return distance(s, navel) < arm;
}

bool isOnThickArc(vec2 s, vec2 navel, float inner, float outer, float sun, float moon) {
  return inner < distance(s, navel)
    && distance(s, navel) < outer
    && radians(-90) < atan(s.y - navel.y, s.x - navel.x)
    && atan(s.y - navel.y, s.x - navel.x) < radians(180);
}

bool isRestmarking(vec2 s, vec2 navel, float arm) {
  return s.x > navel.x - arm
      && s.y > navel.y - arm
      && abs((s.x - (navel.x - arm) + s.y - (navel.y - arm)) - 2*arm) < arm/8;
}

bool isOfSpeechframe(vec2 z) {
  return -7.0/8 < z.x && z.x < 7.0/8 && -7.0/8 < z.y && z.y < -2.0/8;
}

bool isOnSpeechframe(vec2 z, float edge) {
  return -7.0/8+edge < v_position.x && v_position.x < 7.0/8-edge
      && -7.0/8+edge < v_position.y && v_position.y < -2.0/8-edge;
}

void main() {
  float edge = 1.0/80;
  vec2 ear_navel = vec2(7.0/8, 7.0/8);
  float ear_arm = 1.0/16;
  if (isOfSpeechframe(vec2(v_position))) {
    vec3 whelk = isOnSpeechframe(vec2(v_position), edge) ? vec3(53.0, 59.0, 79.0) : vec3(203.0, 203.0, 255.0);
    f_color = vec4(whelk/255, 7.0/8);
  } else if (isRestmarking(v_position.xy, ear_navel, ear_arm)) {
    f_color = vec4(1, 0, 0, 1);
  } else if (
    isOnThickArc(v_position.xy, ear_navel, ear_arm - 4*ear_arm*ear_arm, ear_arm, radians(-90), radians(135))
  || isInCircle(v_position.xy, ear_navel, ear_arm/4)
  ) {
    f_color = vec4(0, 0, 0, 1);
  } else {
    discard;
  }
}
