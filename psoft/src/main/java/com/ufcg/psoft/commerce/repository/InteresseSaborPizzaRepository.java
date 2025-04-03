package com.ufcg.psoft.commerce.repository;

import com.ufcg.psoft.commerce.model.InteresseSaborPizza;
import com.ufcg.psoft.commerce.model.SaborPizza;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface InteresseSaborPizzaRepository extends JpaRepository<InteresseSaborPizza, Long> {
    List<InteresseSaborPizza> findBySaborPizzaAndNotificadoFalse(SaborPizza SaborPizza);
}
